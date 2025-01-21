(ns conta-corrente.core
  (:require [cheshire.core :as che]
            [clojure.java.io :as io]
            [clojure.pprint]
            [taoensso.timbre :as timbre])
  (:gen-class))

(defn trata-erro [db erro]
  (timbre/error erro)
  (assoc db :erros (conj (get db :erros []) erro)))

(defn abertura [db linha]
  (let [conta (:conta linha)]
    (if (contains? (:contas db) conta)
      (trata-erro db (format "Tentativa de abertura para conta já existente '%s'." conta))
      (assoc-in db [:contas conta] {:saldo 0.0 :status "aberta"}))))

(defn fechamento [db linha]
  (let [conta (:conta linha)
        saldo (get-in db [:contas conta :saldo])]
    (if (and (contains? (:contas db) conta) (= saldo 0.0) (= (get-in db [:contas conta :status]) "aberta"))
      (update-in db [:contas conta] assoc :status "fechada")
      (cond (not (contains? (:contas db) conta)) (trata-erro db (format "Tentativa de fechamento falhou conta '%s' inexistente." conta))
            (not= (get-in db [:contas conta :saldo]) 0.0) (trata-erro db (format "Tentativa de fechamento falhou conta '%s' com saldo diferente de 0 (saldo = %s)." conta saldo))
            (= (get-in db [:contas conta :status]) "fechada") (trata-erro db (format "Tentativa de fechamento falhou conta '%s' já está fechada." conta))))))

(defn saque [db linha]
  (let [conta (:conta linha)
        valor-descontar (:valor linha)
        saldo (get-in db [:contas conta :saldo])]
    (if (and (contains? (:contas db) conta) (>= saldo valor-descontar) (= (get-in db [:contas conta :status]) "aberta") (> valor-descontar 0.0))
      (update-in db [:contas conta] assoc :saldo (- saldo valor-descontar))
      (cond (not (contains? (:contas db) conta)) (trata-erro db (format "Tentativa de saque sem sucesso pois conta '%s' inexistente." conta))
            (= (get-in db [:contas conta :status]) "fechada") (trata-erro db (format "Tentativa de saque sem sucesso pois conta '%s' fechada." conta))
            (< saldo valor-descontar) (trata-erro db (format "Tentativa de saque sem sucesso pois saldo conta '%s' insuficiente." conta))
            (< valor-descontar 0.0) (trata-erro db (format "Tentativa de saque sem sucesso pois o valor é negativo, conta '%s'." conta))
            (= valor-descontar 0.0) (trata-erro db (format "Tentativa de saque sem sucesso pois o valor é zero, conta '%s'." conta))))))

(defn deposito [db linha]
  (let [conta (:conta linha)
        valor-adicionar (:valor linha)
        saldo (get-in db [:contas conta :saldo])]
    (if (and (contains? (:contas db) conta) (= (get-in db [:contas conta :status]) "aberta") (> valor-adicionar 0.0))
      (update-in db [:contas conta] assoc :saldo (+ saldo valor-adicionar))
      (cond (not (contains? (:contas db) conta)) (trata-erro db (format "Tentativa de depósito sem sucesso pois conta '%s' é inexistente." conta))
            (= (get-in db [:contas conta :status]) "fechada") (trata-erro db (format "Tentativa de depósito sem sucesso pois conta '%s' está fechada." conta))
            (< valor-adicionar 0.0) (trata-erro db (format "Tentativa de depósito sem sucesso pois o valor é negativo, conta '%s'." conta))
            (= valor-adicionar 0.0) (trata-erro db (format "Tentativa de depósito sem sucesso pois o valor é zero, conta '%s'." conta))))))

; Define a estrutura de impressão no arquivo de um relatório
(defn conteudo-relatorio [db linha]
  (let [comando (:tipo-cmd linha) conta (:conta linha)]
   (cond
     (empty? db)
     "Banco sem registros"
     (= comando "relatorio-geral")
     (str (into [] (:contas db)))
     (= comando "relatorio-conta")
     (if (empty? (get-in db [:contas conta] []))
       "Erro, conta inexistente"
       (str [conta (get-in db [:contas conta])]))
     :else
     "Erro, tipo de relatório desconhecido")))

; Imprime o estado atual de todas as contas em um arquivo
(defn relatorio-geral! [db linha]
  (doall
   (with-open
    [wrtr (io/writer (str "rel/relatorio-geral-" (java.lang.System/currentTimeMillis) ".txt"))]
     (.write wrtr (conteudo-relatorio db linha))))
  db)

; Imprime o estado atual de uma contas em um arquivo
(defn relatorio-conta! [db linha]
  (doall
   (with-open [wrtr (io/writer (str "rel/relatorio-conta-" (:conta linha) "-" (java.lang.System/currentTimeMillis) ".txt"))]
     (.write wrtr (conteudo-relatorio db linha))))
  db)

(defn operacao [db linha]
  (case (:tipo-cmd linha)
    "abertura" (abertura db linha)
    "fechamento" (fechamento db linha)
    "saque" (saque db linha)
    "deposito" (deposito db linha)
    "relatorio-geral" (relatorio-geral! db linha)
    "relatorio-conta" (relatorio-conta! db linha)
    :else (format "Comando desconhecido! '%s'." (:tipo-cmd linha))))

(defn json-map [linha]
  (if (= linha "") "Erro, comando vazio."
   (che/parse-string linha true)))

;;-----------------------------------------------------------------------------------------
(defn ler-arquivo [arquivo]

  (with-open [rdr (io/reader arquivo)]

    (loop [linhas (line-seq rdr) ;;loop que pega a line -> ("linha1" "linha2" "linha3")
           contas {}]

      (if (empty? linhas) contas (recur
                                  (next linhas)
                                  (operacao contas (json-map (first linhas))))))))
;;-----------------------------------------------------------------------------------------

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (ler-arquivo (first args)))
