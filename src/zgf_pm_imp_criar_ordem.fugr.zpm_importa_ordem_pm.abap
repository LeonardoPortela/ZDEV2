FUNCTION zpm_importa_ordem_pm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ONLINE) TYPE  CHAR1
*"     REFERENCE(ORDENS) TYPE  ZTPM_D_M_ORDEM_T
*"  EXPORTING
*"     REFERENCE(RETORNO) TYPE  ZTPM_TRANSA_EXP_ORDEM
*"----------------------------------------------------------------------

  DATA t_ordem TYPE zpmt0016_t.
  DATA t_operacoes TYPE zpmt0017_t.
  DATA t_apontamentos TYPE zpmt0015_t.
  DATA t_notas TYPE zpmt0020_t.
  DATA t_zpmt0020 TYPE zpmt0020_t.
  DATA t_zpmt0017 TYPE zpmt0017_t.
  DATA t_zpmt0015 TYPE zpmt0015_t.
  DATA t_zpmt0016 TYPE zpmt0016_t.
  DATA msg TYPE char255.
  DATA: lt_return        TYPE TABLE OF bapiret2,
        lt_operations    TYPE TABLE OF bapi_alm_order_operation_e,
        lt_itens         TYPE  ztpm_transa_exp_ordem-data-operacoes,
        lt_order_range   TYPE TABLE OF bapi_pp_orderrange,
        lt_confirmations TYPE TABLE OF bapi_conf_key,
        lw_return        TYPE bapiret2,
        lv_pernr         TYPE p_pernr,
        ls_ordem         TYPE zpm_ordem_orc,
        lv_prox_aprov    TYPE string,
        lv_objnr         TYPE ihsg-objnr,
        lt_zpmt0077      TYPE TABLE OF zpmt0077,
        lv_seq           TYPE sy-tabix.

* "// Criando o Objeto da Classe
  DATA(obj_create) = NEW zcl_pm_ordem( ).

* "// Recebendo dos dados da Envio para possiveis alterações
  DATA(it_ordem) = ordens.
* "// Cria uma Sequencia de Envio Chamado bloco
  DATA(nr_bloco) = obj_create->get_bloco( ).

  DATA(_idvnt) = obj_create->get_idvnt( ).

* "// Trada as informações enviada pelo Mobile para adicionar em nossa banco de dados
  LOOP AT it_ordem ASSIGNING FIELD-SYMBOL(<f_ordem>).

    lv_pernr = <f_ordem>-pernr.
* "// Set User do Processo
    obj_create->set_uname( lv_pernr ).
* "// Get User do Processo
    sy-uname = obj_create->get_uname( ).

*   "// Cria uma Id de sequancia da Ordem
    obj_create->set_idord( ).
*   "// Recupera o id gerado anteriormente
    DATA(_idord) = obj_create->get_idord( ).

*   "// Aplica os dados recebidos para minha tabela interna convertendo as possiveis inconsistencias
    APPEND VALUE #(
                    bloco = nr_bloco
                    idord = _idord
                    stato = <f_ordem>-istat
                    aufnr = |{ <f_ordem>-aufnr ALPHA = IN }|
                    auart = <f_ordem>-auart
                    tplnr = <f_ordem>-tplnr
                    pernr = <f_ordem>-pernr
                    ktext = <f_ordem>-ktext
                    iwerk = <f_ordem>-iwerk
                    gewrk = <f_ordem>-objty
                    arbpl = <f_ordem>-arbpl
*                    wergw = <f_ordem>-objid
                    ingpr = <f_ordem>-ingpr
                    ilart = <f_ordem>-ilart
                    equnr = |{ <f_ordem>-equnr ALPHA = IN }|
                    priok = <f_ordem>-priok
                    dtini = <f_ordem>-gstrp
                    dtfim =  <f_ordem>-gltrp
*                    hrini = obj_create->convert_time( <f_ordem>-cadhrinicio )
*                    hrfim = obj_create->convert_time( <f_ordem>-cadhrfinal )
                    dtreg = sy-datum
                    hrreg = sy-uzeit
                    dtult = sy-datum
                    ernam = sy-uname
                    user4 = <f_ordem>-user4
                    text_longo = <f_ordem>-ktext
                    qmnum      = <f_ordem>-qmnum
                   ) TO t_ordem.


*   "// Aplica os dados recebido para a tabela interna
    LOOP AT <f_ordem>-notas ASSIGNING FIELD-SYMBOL(<fs_notas>).
      ADD 1 TO _idvnt.
      APPEND VALUE #(
                      idvnt = |{ _idvnt ALPHA = IN }|
                      bloco = nr_bloco
                      idord = _idord
                      aufnr = |{ <f_ordem>-aufnr ALPHA = IN }|
                      qmnum = |{ <fs_notas>-qmnum ALPHA = IN }|
                    ) TO t_notas.
    ENDLOOP.

*   "// Aplica os dados recebido para a tabela interna
    LOOP AT <f_ordem>-operacao INTO DATA(w_operacao).
*     "// Cria uma sequencia para a Operação
      obj_create->set_idopr( ).
*     "// Recupera o Id anteriormente criado
      DATA(_idopr) = obj_create->get_idopr( ).
*     "// Cria uma VORNR para a Operação
      obj_create->set_vornr( i_ordem = |{ <f_ordem>-aufnr ALPHA = IN }| i_vornr = w_operacao-vornr ).
*     "// Se a Ordem estiver em Branco limpa os VORNR que estiver Preenchido
*      W_OPERACAO-CADVORNR = COND #( WHEN <F_ORDEM>-CADAUFNR IS INITIAL THEN '' ELSE W_OPERACAO-CADVORNR ).
*     "// Recupera o VORNR
*      DATA(_VORNR) = COND #( WHEN W_OPERACAO-CADVORNR IS INITIAL THEN OBJ_CREATE->GET_VORNR( ) ELSE W_OPERACAO-CADVORNR ) .
      DATA(_vornr) = obj_create->get_vornr( ).

*     "// Aplica na tabela interna das Operações
      APPEND VALUE #(
                      bloco = nr_bloco
                      idopr = _idopr
                      idord = _idord
                      vornr = |{ _vornr ALPHA = IN }|
                      arbpl = w_operacao-arbpl
                      werks = w_operacao-werks
                      pernr = w_operacao-pernr
                      ntanf = w_operacao-ntanf
**  Begin of    #106405  FF
*                      steus = COND #( WHEN w_operacao-steus EQ '1' THEN 'PM01' ELSE 'PM03' )
                      steus = w_operacao-steus
** End of FF
                      confn = w_operacao-confnl
                      description = w_operacao-ltxa1
                      ntend = w_operacao-ntend
                      einsa = w_operacao-einsa
                      einse = w_operacao-einse
                      arbei = w_operacao-arbei
                      qmnum = w_operacao-qmnum "FF - 19/04/24 - #131818

                    ) TO t_operacoes.

**     "// Aplica os dados recebido para a tabela interna
      LOOP AT w_operacao-apontamentos ASSIGNING FIELD-SYMBOL(<w_apontamento>).
*       "// Cria uma sequencia para a Apontamento
        obj_create->set_idapn( ).
*       "// Recupera o Id anteriormente criado
        DATA(_idapn) = obj_create->get_idapn( ).
*       "// Aplica na tabela interna de Apontamentos convertendo as possiveis inconsistencias
        APPEND VALUE #(
                        bloco = nr_bloco
                        idord = _idord
                        idopr = _idopr
                        idapn = _idapn
                        rueck = <w_apontamento>-rueck
                        rmzhl = <w_apontamento>-rmzhl
                        pernr = <w_apontamento>-pernr
                        isdd  = <w_apontamento>-isdd
                        isdz  = <w_apontamento>-isdz
                        iedd  = <w_apontamento>-iedd
                        iedz  = <w_apontamento>-iedz
                        grund = <w_apontamento>-grund
                        budat = <w_apontamento>-budat
                        arbpl = <w_apontamento>-arbpl
*                        ersda = <w_apontamento>-ersda
*                        laeda = <w_apontamento>-laeda
                        ismnw = <w_apontamento>-ismnw
                        ismne = 'H'"<W_APONTAMENTO>-ISMNE
                        aueru = <w_apontamento>-aueru
                        vornr = <w_apontamento>-vornr
                        stokz = <w_apontamento>-stokz
                      ) TO t_apontamentos.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  PERFORM f_valida_apontamentos USING t_apontamentos
                                      it_ordem
                             CHANGING msg.

  IF msg IS NOT INITIAL.
*     "// Autera o Status do Processamento para "E" de Erro
*    <_ordem>-statp = 'E'.  "// Erro no Processamento
*     "// Aplica na Tebela Fisica
*    MODIFY zpmt0016 FROM <_ordem>.
*     "// retorna a mensagem de Erro
    retorno-erros-msg_erros = msg.
    retorno-data =
     VALUE #(
              nr_ordem  = ''
              status    = ''
              msg_ordem = msg
            ).

    EXIT.
  ENDIF.


* "//Aplica as Tabelas internas na Fisica
  MODIFY zpmt0016 FROM TABLE t_ordem.         " Ordens
  MODIFY zpmt0017 FROM TABLE t_operacoes.     " Operações
  MODIFY zpmt0015 FROM TABLE t_apontamentos.  " Apontamentos
  MODIFY zpmt0020 FROM TABLE t_notas.         " Notas
  COMMIT WORK.

  sy-uname = obj_create->get_uname( ).

* "// Validações de Negocio
  CALL METHOD obj_create->verifica_erros
    EXPORTING
      i_ordem    = t_ordem
      i_operacao = t_operacoes
    RECEIVING
      i_erros    = DATA(erros).

  LOOP AT erros INTO DATA(_erros).
    IF msg IS INITIAL.
      msg = _erros-message.
    ELSE.
      msg = |{ msg }, { _erros-message }|.
    ENDIF.
  ENDLOOP.

  retorno-erros-msg_erros = msg.

  CHECK msg IS INITIAL.

* "// Verifica a Quantidade de itens do Bloco executado
  DATA(qtd_item) = obj_create->check_qtd_item_bloco( nr_bloco ).

* "// Em caso não esteja Online o Sistema retornara as mensagens
  IF online IS INITIAL.
*   "// Pega aquantidade de linhas enviada pelo Mobile
    DATA(lines) = lines( it_ordem ).
*   "// Compara essa quantidade de Linhas do Mobile com as linhas armazenadas na tatela Fisica
    IF qtd_item EQ lines.
      retorno-sucess = abap_true.
      retorno-data =
      VALUE #(
                nr_ordem  = ''
                status    = ''
                msg_ordem = COND #( WHEN qtd_item EQ 1
                                                      THEN |Foi armazenado { qtd_item } item com sucesso!|
                                                      ELSE |Foram armazenados { qtd_item } itens com sucesso!| )
              ).
    ELSE.
      retorno-sucess = abap_false.
      retorno-data =
      VALUE #(
               nr_ordem  = ''
               status    = '210'
               msg_ordem = COND #( WHEN qtd_item EQ 1
               THEN |Foi armazenado somente { qtd_item } de { COND #( WHEN lines EQ 1
                                                                           THEN |{ lines } item!|
                                                                           ELSE |{ lines } itens!| ) } |
               ELSE |Foram armazenados somente { qtd_item } de { lines } itens!| )
             ).
    ENDIF.
**   "// Processa os Dados que foi Enviado pelo Mobile
    CALL METHOD obj_create->call_report
      EXPORTING
        i_sequen = CONV #( |{ nr_bloco ALPHA = OUT }| )
        i_report = 'ZPMR0045'.
*   "// Finaliza o processo em caso de dados OffLine
    EXIT.
  ENDIF.

* "// Se OnLine continua o processo
* "// seleciona os dados da Ordem pegando somente os que foi enviado no momento pelo Mobile
  FREE t_zpmt0016.
  SELECT *
     FROM zpmt0016
      INTO TABLE t_zpmt0016
    FOR ALL ENTRIES IN t_ordem
      WHERE bloco EQ t_ordem-bloco.

  IF t_zpmt0016 IS NOT INITIAL.
*   "// Seleciona as Operações com as Ordens Selecionadas Anteriormente
    SELECT *
         FROM zpmt0017
          INTO TABLE t_zpmt0017
        FOR ALL ENTRIES IN t_zpmt0016
          WHERE bloco EQ t_zpmt0016-bloco
          AND idord EQ t_zpmt0016-idord.

    SELECT *
      FROM zpmt0020
      INTO TABLE t_zpmt0020
        FOR ALL ENTRIES IN t_zpmt0016
          WHERE bloco EQ t_zpmt0016-bloco
          AND idord EQ t_zpmt0016-idord.

    IF t_zpmt0017 IS NOT INITIAL.
*     "// Seleciona os Apontamentos com as Operações Selecionadas Anteriormente
      SELECT *
           FROM zpmt0015
            INTO TABLE t_zpmt0015
          FOR ALL ENTRIES IN t_zpmt0017
            WHERE bloco EQ t_zpmt0017-bloco
            AND idord EQ t_zpmt0017-idord
            AND idopr EQ t_zpmt0017-idopr.
    ENDIF.
  ENDIF.

* "// Processa as Ordem Enviadas
  LOOP AT t_zpmt0016 ASSIGNING FIELD-SYMBOL(<_ordem>).
*   "// Verifica se é Change ou Create
    DATA(_update) = COND #( WHEN <_ordem>-aufnr IS NOT INITIAL THEN abap_true ELSE abap_false ).
*   "// Chama a BABI para Criar/Modificar as Ordens
    CALL METHOD obj_create->criar_ordens
      EXPORTING
        i_ordem        = <_ordem>      " Estrutura Ordem
        i_operacao     = t_zpmt0017    " Tabela de Operações
        i_apontamento  = t_zpmt0015    " Tabela de Apontamento
        i_notas        = t_zpmt0020
        i_online       = online
      IMPORTING
        e_ordem        = DATA(_ordem)  " Retorno da Ordem
        e_msg          = DATA(_msg)   " Retorno da mensagen de Erro/Sucesso
        e_apontamentos = DATA(lt_apont_retorno).

    IF <_ordem>-aufnr IS NOT INITIAL AND _ordem IS INITIAL.
      _ordem = <_ordem>-aufnr.
    ENDIF.

    IF <_ordem>-aufnr IS INITIAL AND _ordem IS INITIAL.
      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING FIELD-SYMBOL(<fs_zpmt0077>).
      <fs_zpmt0077>-aufnr = _ordem.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'ORDEM'.
      <fs_zpmt0077>-observacao =  'Sem AUFNR'.
    ENDIF.

*   "// Ordem preenchida
    IF _ordem IS NOT INITIAL.

*     "// Vincula a Ordem na Nota
      LOOP AT t_zpmt0020 ASSIGNING FIELD-SYMBOL(<f_nota>).
        <f_nota>-aufnr = _ordem.
      ENDLOOP.
*     "// Preenche a ordem com ZEROS
      <_ordem>-aufnr = |{ _ordem ALPHA = IN }|.
*     "// Autera o Status do Processamento para "P" de Processado
      <_ordem>-statp = 'P'.  "// Processado
*     "// Aplica na Tabela Fisica
      MODIFY zpmt0016 FROM <_ordem>.
      MODIFY zpmt0020 FROM TABLE t_zpmt0020.

      APPEND INITIAL LINE TO lt_order_range ASSIGNING FIELD-SYMBOL(<fs_range>).
      <fs_range>-sign   = 'I'.
      <fs_range>-option = 'EQ'.
      <fs_range>-low    = _ordem.

      IF t_operacoes IS NOT INITIAL.

        IF lt_apont_retorno IS NOT INITIAL.

          LOOP AT lt_apont_retorno ASSIGNING FIELD-SYMBOL(<fs_apont_ret>).

            retorno-erros = <fs_apont_ret>-msg_erro.
          ENDLOOP.

        ENDIF.
        LOOP AT t_operacoes ASSIGNING FIELD-SYMBOL(<fs_operations>).

          APPEND INITIAL LINE TO lt_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).
          <fs_itens>-numero = <fs_operations>-vornr.

          CALL FUNCTION 'BAPI_ALM_CONF_GETLIST'
            EXPORTING
              operation     = <fs_operations>-vornr
            IMPORTING
              return        = lw_return
            TABLES
              order_range   = lt_order_range
              confirmations = lt_confirmations.

          LOOP AT lt_confirmations ASSIGNING FIELD-SYMBOL(<fs_confirmations>).

            IF <fs_confirmations>-rev_conf_cnt IS NOT INITIAL.
              CONTINUE.
            ENDIF.

            APPEND INITIAL LINE TO <fs_itens>-apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).
            <fs_apontamentos>-confirmacao = <fs_confirmations>-conf_cnt.
            <fs_apontamentos>-item        = <fs_confirmations>-conf_no.

          ENDLOOP.

          REFRESH: lt_confirmations,
                   lt_return.
        ENDLOOP.
      ENDIF.


*      DELETE retorno-erros WHERE msg_erros IS INITIAL.
*     "// retorna a mensagem de Sucesso
      retorno-sucess = abap_true.
      retorno-data =
       VALUE #(
                nr_ordem  = <_ordem>-aufnr
                status    = ''
                msg_ordem = |Ordem { <_ordem>-aufnr } { COND #( WHEN _update IS NOT INITIAL THEN 'atualizada' ELSE 'criada' ) } com Sucesso!|
                operacoes = lt_itens
              ).
    ELSE.
      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
      <fs_zpmt0077>-aufnr = _ordem.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'ORDEM'.
      <fs_zpmt0077>-observacao =  'Erro na execução do MÉTODO de criação/atualização'.

*     "// Autera o Status do Processamento para "E" de Erro
      <_ordem>-statp = 'E'.  "// Erro no Processamento
*     "// Aplica na Tebela Fisica
      MODIFY zpmt0016 FROM <_ordem>.
*     "// retorna a mensagem de Erro

      retorno-erros-msg_erros = _msg.
      retorno-data =
       VALUE #(
                nr_ordem  = ''
                status    = ''
              ).
    ENDIF.
  ENDLOOP.

  IF lt_zpmt0077 IS NOT INITIAL.
    MODIFY zpmt0077 FROM TABLE lt_zpmt0077.
  ENDIF.


  COMMIT WORK.

ENDFUNCTION.


*var cadaufnr    ORDERID     String, /* Código da Ordem */
*var cadauart    ORDER_TYPE  String, /* Tipo de Ordem */
*var cadautyp    String, /* Categoria de Ordem*/ ####
*var cadtplnr    FUNCT_LOC   String, /* Local de Instalação */
*var cadequnr    EQUIPMENT   String, /* Equipamento */
*var cadktext    SHORT_TEXT  String, /* Texto do Evento */
*var cadiwerk    PLANPLANT   String, /* Centro do Grupo de Planejamento */
*var cadingrp    PLANGROUP   String, /* Grupo de Planejamento */
*var cadilart    PMACTTYPE   String, /* Tipo de atividade de manutenção */
*var cadobjty    MN_WK_CTR   String, /* Centro de Trabalho - Categoria de objeto para recurso CIM */
*var cadobjid    PLANT       String, /* Centro de Trabalho - Identificação de objeto para recurso */
*var caddtinicio START_DATE  String, /* Data de Início */
*var cadhrinicio BASICSTART  String, /* Hora de Início */
*var caddtfinal  FINISH_DATE String, /* Data de Final */
*var cadhrfinal  BASIC_FIN   String, /* Hora de Final */
*var cadartpr    String, /* Tipo de prioridade */ ########
*var cadpriok    PRIORITY    String, /* Prioridade */
*var cadpernr    String, /* CPF do Usuario */    #########
*var cadistat    String  /*  0 - Não Liberado / 1 - Liberado */  #######

*var cadiwerk   PLANT       String,  /*Centro de da Ordem*/
*var cadvornr   ACTIVITY    String,  /*Nr de Item da Operação*/
*var cadltxa1   DESCRIPTION String,  /*Texto da Operação*/
*var cadwerks   WORK_CNTR   String,  /* Centro do Centro de Trabalho */
*var cadobjty   String,  /* Centro de Trabalho - Categoria de objeto para recurso CIM */ ####
*var cadobjid   String,  /* Centro de Trabalho - Identificação de objeto para recurso */ ####
*var cadntanf   String,  /* Data de Lançamento */ #####
*var cadsteus   CONTROL_KEY String,     /*Interno/Externo*/  == PM01 / PM03 #####
*var cadpernr   String   /* CPF do Usuario */ #####


*START_DATE
*FINISH_DATE
*BASICSTART
*BASIC_FIN
