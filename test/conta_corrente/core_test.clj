(ns conta-corrente.core-test
  (:require [clojure.test :refer :all]
            [conta-corrente.core :refer :all]))

(deftest json-map-test
  (testing "[OK] Transform a JSON into a Clojure map"
    (is (= (json-map "{\"tipo-cmd\":\"abertura\", \"conta\":\"00000001\", \"saldo\":500.00}")
         {:tipo-cmd "abertura" :conta "00000001" :saldo 500.00})))
  (testing "[ERRO] Transform a JSON into a Clojure map"
    (is (= (json-map "")
         "Erro, comando vazio."))))

(deftest abertura-test
  (testing "Caso da operação abertura perfeita"
    (is (= (abertura {} {:tipo-cmd "abertura"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "aberta"}}})))
  
  (testing "Caso da operação abertura conta já existente"
    (is (= (abertura {:contas {"00000001" {:saldo 0.0 :status "aberta"}}} {:tipo-cmd "abertura"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "aberta"}} :erros ["Tentativa de abertura para conta já existente '00000001'."]})))
  
  (testing "Caso da operação abertura de uma segunda conta inexistente"
    (is (= (abertura {:contas {"00000001" {:saldo 0.0 :status "aberta"}}} 
                     {:tipo-cmd "abertura"
                           :agencia "001"
                           :conta "00000002"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "aberta"} "00000002" {:saldo 0.0 :status "aberta"}}})))
    )

(deftest fechamento-test
  (testing "Caso da operação fechamento que não existe"
    (is (= (fechamento {} {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:erros ["Tentativa de fechamento falhou conta '00000001' inexistente."]})))
  
  (testing "Caso da operação fechamento de conta existente e saldo zerado."
    (is (= (fechamento {:contas {"00000001" {:saldo 0.0 :status "aberta"}}} 
                     {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "fechada"}}})))
  
  (testing "Caso da operação fechamento de conta existente mas saldo não zerado (positivo)."
    (is (= (fechamento {:contas {"00000001" {:saldo 10 :status "aberta"}}} 
                     {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 10 :status "aberta"}} :erros ["Tentativa de fechamento falhou conta '00000001' com saldo diferente de 0 (saldo = 10)."]})))
  (testing "Caso da operação fechamento de conta com mais de uma conta no banco de dados."
    (is (= (fechamento {:contas {"00000001" {:saldo 0.0 :status "aberta"} "00000002" {:saldo 0.0 :status "aberta"}}} 
                     {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 0.0 :status "aberta"}}})))
  (testing "Caso da operação fechamento de conta já fechada."
    (is (= (fechamento {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 0.0 :status "aberta"}}} 
                     {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 0.0 :status "aberta"}} :erros ["Tentativa de fechamento falhou conta '00000001' já está fechada."]})))
  (testing "Caso da operação fechamento de conta já fechada contento um erro no vetor de erros."
    (is (= (fechamento {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 0.0 :status "aberta"}} :erros ["Tentativa de fechamento falhou conta '00000001' já está fechada."]} 
                     {:tipo-cmd "fechamento"
                           :agencia "001"
                           :conta "00000001"
                           :ts "2020-05-15T17:40:41.428891+00:00"
                           :titular "Fabricio Benedito Destro"
                           :cpf "12345678900"
                           :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 0.0 :status "aberta"}} :erros ["Tentativa de fechamento falhou conta '00000001' já está fechada." "Tentativa de fechamento falhou conta '00000001' já está fechada."]})))
    )

(deftest saque-test
  (testing "Caso da operação saque perfeita (conta existente, saldo suficiente e status aberta)."
    (is (= (saque {:contas {"00000001" {:saldo 100.0 :status "aberta"}}} 
                  {:tipo-cmd "saque"
                   :agencia "001"
                   :conta "00000001"
                   :ts "2020-05-15T17:40:41.428891+00:00"
                   :titular "Fabricio Benedito Destro"
                   :cpf "12345678900"
                   :valor 10.00
                   :status "aberta"})
           {:contas {"00000001" {:saldo 90.0 :status "aberta"}}})))
  
  (testing "Caso da operação saque sem sucesso pois conta inexistente."
    (is (= (saque {:contas {"00000002" {:saldo 100.0 :status "aberta"}}} 
                  {:tipo-cmd "saque"
                   :agencia "001"
                   :conta "00000001"
                   :ts "2020-05-15T17:40:41.428891+00:00"
                   :titular "Fabricio Benedito Destro"
                   :cpf "12345678900"
                   :valor 10.00
                   :status "aberta"})
           {:contas {"00000002" {:saldo 100.0 :status "aberta"}} :erros ["Tentativa de saque sem sucesso pois conta '00000001' inexistente."]})))
  
  (testing "Caso da operação saque sem sucesso pois conta com saldo insuficiente."
    (is (= (saque {:contas {"00000002" {:saldo 100.0 :status "aberta"}}} 
                  {:tipo-cmd "saque"
                   :agencia "001"
                   :conta "00000002"
                   :ts "2020-05-15T17:40:41.428891+00:00"
                   :titular "Fabricio Benedito Destro"
                   :cpf "12345678900"
                   :valor 200.00
                   :status "aberta"})
           {:contas {"00000002" {:saldo 100.0 :status "aberta"}} :erros ["Tentativa de saque sem sucesso pois saldo conta '00000002' insuficiente."]})))
  
  (testing "Caso da operação saque sem sucesso pois conta com status fechada."
    (is (= (saque {:contas {"00000002" {:saldo 0.0 :status "fechada"}}} 
                  {:tipo-cmd "saque"
                   :agencia "001"
                   :conta "00000002"
                   :ts "2020-05-15T17:40:41.428891+00:00"
                   :titular "Fabricio Benedito Destro"
                   :cpf "12345678900"
                   :valor 200.00
                   :status "aberta"})
           {:contas {"00000002" {:saldo 0.0 :status "fechada"}} :erros ["Tentativa de saque sem sucesso pois conta '00000002' fechada."]})))
  (testing "Caso da operação saque perfeita (conta existente, saldo suficiente e status aberta)."
    (is (= (saque {:contas {"00000001" {:saldo 100.0 :status "aberta"} "00000002" {:saldo 100.0 :status "aberta"}}} 
                  {:tipo-cmd "saque"
                   :agencia "001"
                   :conta "00000001"
                   :ts "2020-05-15T17:40:41.428891+00:00"
                   :titular "Fabricio Benedito Destro"
                   :cpf "12345678900"
                   :valor 10.00
                   :status "aberta"})
           {:contas {"00000001" {:saldo 90.0 :status "aberta"} "00000002" {:saldo 100.0 :status "aberta"}}})))
  (testing "Caso da operação saque sem sucesso pois foi usado valor negativo."
      (is (= (saque {:contas {"00000001" {:saldo 50.0 :status "aberta"}}}
                    {:tipo-cmd "saque"
                     :agencia "001"
                     :conta "00000001"
                     :ts "2020-05-15T17:40:41.428891+00:00"
                     :titular "Fabricio Benedito Destro"
                     :cpf "12345678900"
                     :valor -10.00
                     :status "aberta"})
             {:contas {"00000001" {:saldo 50.0 :status "aberta"}} :erros ["Tentativa de saque sem sucesso pois o valor é negativo, conta '00000001'."]})))
    (testing "Caso da operação saque sem sucesso pois foi usado valor zero."
      (is (= (saque {:contas {"00000001" {:saldo 50.0 :status "aberta"}}}
                    {:tipo-cmd "saque"
                     :agencia "001"
                     :conta "00000001"
                     :ts "2020-05-15T17:40:41.428891+00:00"
                     :titular "Fabricio Benedito Destro"
                     :cpf "12345678900"
                     :valor 0.00
                     :status "aberta"})
             {:contas {"00000001" {:saldo 50.0 :status "aberta"}} :erros ["Tentativa de saque sem sucesso pois o valor é zero, conta '00000001'."]}))))
  
(deftest deposito-test
  (testing "Caso da operação deposito perfeita com saldo 0 (conta existente e status aberta)."
    (is (= (deposito {:contas {"00000001" {:saldo 0.0 :status "aberta"}}}
                     {:tipo-cmd "deposito"
                      :agencia "001"
                      :conta "00000001"
                      :ts "2020-05-15T17:40:41.428891+00:00"
                      :titular "Fabricio Benedito Destro"
                      :cpf "12345678900"
                      :valor 10.00
                      :status "aberta"})
           {:contas {"00000001" {:saldo 10.0 :status "aberta"}}})))

  (testing "Caso da operação deposito perfeita com saldo diferente de 0 (conta existente e status aberta)."
    (is (= (deposito {:contas {"00000001" {:saldo 10.0 :status "aberta"}}}
                     {:tipo-cmd "deposito"
                      :agencia "001"
                      :conta "00000001"
                      :ts "2020-05-15T17:40:41.428891+00:00"
                      :titular "Fabricio Benedito Destro"
                      :cpf "12345678900"
                      :valor 10.00
                      :status "aberta"})
           {:contas {"00000001" {:saldo 20.0 :status "aberta"}}})))
  (testing "Caso da operação deposito sem sucesso pois conta inexistente ."
    (is (= (deposito {:contas {"00000001" {:saldo 10.0 :status "aberta"}}}
                     {:tipo-cmd "deposito"
                      :agencia "001"
                      :conta "00000002"
                      :ts "2020-05-15T17:40:41.428891+00:00"
                      :titular "Fabricio Benedito Destro"
                      :cpf "12345678900"
                      :valor 10.00
                      :status "aberta"})
           {:contas {"00000001" {:saldo 10.0 :status "aberta"}} :erros ["Tentativa de depósito sem sucesso pois conta '00000002' é inexistente."]})))

  (testing "Caso da operação deposito sem sucesso pois conta existe mas o status é fechada."
    (is (= (deposito {:contas {"00000001" {:saldo 0.0 :status "fechada"}}}
                     {:tipo-cmd "deposito"
                      :agencia "001"
                      :conta "00000001"
                      :ts "2020-05-15T17:40:41.428891+00:00"
                      :titular "Fabricio Benedito Destro"
                      :cpf "12345678900"
                      :valor 10.00
                      :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "fechada"}} :erros ["Tentativa de depósito sem sucesso pois conta '00000001' está fechada."]})))
  (testing "Caso da operação deposito sem sucesso pois foi usado valor negativo."
    (is (= (deposito {:contas {"00000001" {:saldo 0.0 :status "aberta"}}}
                     {:tipo-cmd "deposito"
                      :agencia "001"
                      :conta "00000001"
                      :ts "2020-05-15T17:40:41.428891+00:00"
                      :titular "Fabricio Benedito Destro"
                      :cpf "12345678900"
                      :valor -10.00
                      :status "aberta"})
           {:contas {"00000001" {:saldo 0.0 :status "aberta"}} :erros ["Tentativa de depósito sem sucesso pois o valor é negativo, conta '00000001'."]})))
      (testing "Caso da operação deposito sem sucesso pois foi usado valor zero."
      (is (= (deposito {:contas {"00000001" {:saldo 50.0 :status "aberta"}}}
                    {:tipo-cmd "deposito"
                     :agencia "001"
                     :conta "00000001"
                     :ts "2020-05-15T17:40:41.428891+00:00"
                     :titular "Fabricio Benedito Destro"
                     :cpf "12345678900"
                     :valor 0.00
                     :status "aberta"})
             {:contas {"00000001" {:saldo 50.0 :status "aberta"}} :erros ["Tentativa de depósito sem sucesso pois o valor é zero, conta '00000001'."]}))))

(deftest conteudo-relatorio-test
  (testing "conteudo-relatorio para relatório geral perfeito em uma conta."
    (is (= (conteudo-relatorio {:contas {"00000001" {:saldo 0.0 :status "fechada"}}}
                               {:tipo-cmd "relatorio-geral"
                                :agencia "001"
                                :conta "00000001"
                                :ts "2020-05-15T17:40:41.428891+00:00"
                                :titular "Fabricio Benedito Destro"
                                :cpf "12345678900"
                                :valor 10.00
                                :status "aberta"})
           "[[\"00000001\" {:saldo 0.0, :status \"fechada\"}]]")))
  (testing "conteudo-relatorio para relatório de conta perfeito."
    (is (= (conteudo-relatorio {:contas {"00000001" {:saldo 0.0 :status "fechada"}}}
                               {:tipo-cmd "relatorio-conta"
                                :agencia "001"
                                :conta "00000001"
                                :ts "2020-05-15T17:40:41.428891+00:00"
                                :titular "Fabricio Benedito Destro"
                                :cpf "12345678900"
                                :valor 10.00
                                :status "aberta"})
           "[\"00000001\" {:saldo 0.0, :status \"fechada\"}]")))
  (testing "conteudo-relatorio para relatório geral perfeito em mais de uma conta."
    (is (= (conteudo-relatorio {:contas {"00000001" {:saldo 0.0 :status "fechada"} "00000002" {:saldo 10.0 :status "aberta"}}}
                               {:tipo-cmd "relatorio-geral"
                                :agencia "001"
                                :conta "00000001"
                                :ts "2020-05-15T17:40:41.428891+00:00"
                                :titular "Fabricio Benedito Destro"
                                :cpf "12345678900"
                                :valor 10.00
                                :status "aberta"})
           "[[\"00000001\" {:saldo 0.0, :status \"fechada\"}] [\"00000002\" {:saldo 10.0, :status \"aberta\"}]]")))
  (testing "conteudo-relatorio para relatório geral com banco vazio."
    (is (= (conteudo-relatorio {}
                               {:tipo-cmd "relatorio-geral"
                                :agencia "001"
                                :conta "00000001"
                                :ts "2020-05-15T17:40:41.428891+00:00"
                                :titular "Fabricio Benedito Destro"
                                :cpf "12345678900"
                                :valor 10.00
                                :status "aberta"})
           "Banco sem registros")))
  (testing "conteudo-relatorio para relatório de conta para conta inexistente."
    (is (= (conteudo-relatorio {:contas {"00000001" {:saldo 0.0 :status "fechada"}}}
            {:tipo-cmd "relatorio-conta"
             :agencia "001"
             :conta "00000002"
             :ts "2020-05-15T17:40:41.428891+00:00"
             :titular "Fabricio Benedito Destro"
             :cpf "12345678900"
             :valor 10.00
             :status "aberta"})
           "Erro, conta inexistente")))
  (testing "conteudo-relatorio para relatório de conta com banco vazio."
    (is (= (conteudo-relatorio {}
                               {:tipo-cmd "relatorio-conta"
                                :agencia "001"
                                :conta "00000001"
                                :ts "2020-05-15T17:40:41.428891+00:00"
                                :titular "Fabricio Benedito Destro"
                                :cpf "12345678900"
                                :valor 10.00
                                :status "aberta"})
           "Banco sem registros"))))
