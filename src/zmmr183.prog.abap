**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Ronaldo Freitas ( ronaldo.freitas@amaggi.com.br )                    |*
**|                                                                           |*
**|  PO/Tester:                                                               |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br )                  |*
**|    + Amaury Silva ( amaury.silva@amaggi.com.br )                          |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Gerar Fluxo de Arrendamentos                                              |*
**/===========================================================================\*
*&---------------------------------------------------------------------*
*& Report  ZMMR183
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr183.

TABLES zib_cte_dist_ter.

TYPES: BEGIN OF ty_info_forne.
TYPES: bvtyp TYPE bvtyp,    "Tipo de banco do parceiro
       texto TYPE char50,   "Fornecedor
       bankl TYPE char03,   "Banco
       banka TYPE banka,    "Nome do Banco
       bankn TYPE bankn,    "Conta Corrente
       agenc TYPE char15.   "Agencia
TYPES: END OF ty_info_forne.

TYPES: BEGIN OF ty_inf_forn,
         lifnr TYPE lifnr,    "
         zterm TYPE dzterm,   "
         ztag1 TYPE dztage.   "Condição pgto
TYPES: END OF ty_inf_forn.

TYPES:  BEGIN OF ty_cte_alv,
          ck_chegada_doc     TYPE char01,
          status             TYPE char04,
          p_emissor_n        TYPE name1_gp,
          zvalor_ft_peso     TYPE zde_valor_frete_ton,
          zvalor_vi_peso     TYPE	zde_valor_vi_ton,
          cd_matnr_nota	     TYPE zde_matnr_nota,
          tx_prod_nota       TYPE zde_tx_prod_nota,
          tx_grupo_merc_nota TYPE zde_tx_gb_merc_nota.
          INCLUDE STRUCTURE zib_cte_dist_ter.
TYPES: END OF ty_cte_alv.

TYPES: BEGIN OF ty_bloq.
TYPES:   cd_chave_cte TYPE zde_chave_doc_e.
TYPES: END OF ty_bloq.


DATA: e_lfbk  TYPE lfbk,
      vg_cont TYPE p,
      e_bnka  TYPE bnka.

*&--------------------------Declarações--------------------------------*
DATA: it_rsparams        TYPE TABLE OF rsparams,
      it_forn            TYPE TABLE OF ty_inf_forn,
      ws_forn            TYPE ty_inf_forn,
      it_alv_saida       TYPE TABLE OF ty_cte_alv,
      it_entrada         TYPE TABLE OF zde_cte_dist_alv,
      wa_entrada         TYPE zde_cte_dist_alv,
      wa_rsparams        TYPE rsparams,
      wa_cte_dist_ter    TYPE zib_cte_dist_ter,
      lc_seq             TYPE zde_seq_log,
      it_n55_t           TYPE zib_cte_dist_n55_t,
      it_chaves_bloq     TYPE TABLE OF ty_bloq WITH HEADER LINE,
      wa_n55_t           TYPE zib_cte_dist_n55,
      wa_info_forne      TYPE ty_info_forne,
      wa_cte_select      TYPE zib_cte_dist_ter,
      lr_data            TYPE REF TO data,
      lv_estornar        TYPE char01,
      it_cte             TYPE zib_cte_dist_ter_t,
      it_alv             TYPE zde_cte_dist_alv_t,
      wa_alv             TYPE zde_cte_dist_alv,
      lr_data_line       TYPE REF TO data,
      r_msg_erro         TYPE c,
      lr_data_descr      TYPE REF TO cl_abap_datadescr,
      lr_data_line_descr TYPE REF TO cl_abap_datadescr,
      zvencimento        TYPE p,
      zdatevenc          TYPE sy-datum,
      zcond_pto          TYPE c,
      vg_dias            TYPE p,
      obj_cte            TYPE REF TO zcl_cte_dist_g.

DATA: r_chave TYPE RANGE OF zde_chave_doc_e.

FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
               <lt_data_line> TYPE ANY TABLE,
               <ls_data>      TYPE any,
               <ls_data_line> TYPE any.

DATA: number           TYPE tbtcjob-jobcount,
      name             TYPE tbtcjob-jobname VALUE 'ZMM0079_FRETE_FERROVIARIO',
      print_parameters TYPE pri_params.

DATA: wa_0301_ter TYPE TABLE OF zib_cte_dist_ter    WITH HEADER LINE,
      it_0301_n55 TYPE TABLE OF zib_cte_dist_n55    WITH HEADER LINE,
      it_0301_n01 TYPE TABLE OF zib_cte_dist_n01    WITH HEADER LINE,
      it_0301_nit TYPE TABLE OF zib_cte_dist_nit    WITH HEADER LINE,
      it_0301_dup TYPE TABLE OF zib_cte_dist_dup    WITH HEADER LINE,
      it_0301_vt  TYPE TABLE OF zde_cte_dist_vt_alv WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK ctedata WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:   pproc  FOR zib_cte_dist_ter-tp_processo_cte,
                    etomad FOR zib_cte_dist_ter-e_tomadora,
                    chavec FOR zib_cte_dist_ter-cd_chave_cte,
                    pmodal FOR zib_cte_dist_ter-cd_modal,
                    dtemit FOR zib_cte_dist_ter-dt_emissao. "DEFAULT SY-DATUM,
*PARAMETER: ck_canc AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK ctedata.


INITIALIZATION.
*  APPEND VALUE #( sign = 'I' option = 'BT' low = sy-datum - 1000 high = sy-datum ) TO dtemit.

START-OF-SELECTION.

*-------------------------------------------
* execute ZPM0014
*-------------------------------------------

  PERFORM: submit_zmm0079.


*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_prepare_run_time_info USING p_display TYPE c..

  IF <lt_data> IS ASSIGNED.
    CLEAR: <lt_data>[].
  ENDIF.

  IF <lt_data_line> IS ASSIGNED.
    CLEAR: <lt_data_line>[].
  ENDIF.

  IF <ls_data> IS ASSIGNED.
    CLEAR: <ls_data>.
  ENDIF.

  IF <ls_data_line> IS ASSIGNED.
    CLEAR: <ls_data_line>.
  ENDIF.

  FREE: lr_data, lr_data_line, lr_data_descr, lr_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = p_display
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_runtime_info .

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data_descr      = lr_data_descr
          r_data_line_descr = lr_data_line_descr ).

      CHECK ( lr_data_descr IS NOT INITIAL ) OR ( lr_data_line_descr IS NOT INITIAL ).

      CREATE DATA lr_data      TYPE HANDLE lr_data_descr.
      CREATE DATA lr_data_line TYPE HANDLE lr_data_line_descr.

      ASSIGN lr_data->*      TO <lt_data>.
      ASSIGN lr_data_line->* TO <lt_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <lt_data>
                                                   t_data_line = <lt_data_line> ).

    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN lr_data->*      TO <ls_data>.
  ASSIGN lr_data_line->* TO <ls_data_line>.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZMM0079
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_zmm0079 .

*  Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.


  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '07'. "Gerar VT e VI
  IF sy-subrc EQ 0.


*&--------------------------Log. Principal-----------------------------*

    "Calcular data inicio para seleção.
    IF dtemit IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'BT' low = sy-datum - 11 high = sy-datum - 1 ) TO dtemit.
    ENDIF.

    PERFORM f_prepare_run_time_info USING abap_false.       "ZMM0079
    SUBMIT zmmr105
     WITH ck_01 EQ '' "Todos / Informações de estatus documento
     WITH ck_02 EQ 'X' "Pendentes / Informações de estatus documento
     WITH ck_03 EQ ''  "Finalizado / Informações de estatus documento
     WITH ck_a1 EQ 'X' "Check autorização pagamento grupo material / Todos
     WITH ck_a2 EQ ''  "Check autorização pagamento grupo material / Autorizado
     WITH ck_a3 EQ ''  "Check autorização pagamento grupo material / Pendentes
     WITH ck_auto  EQ 'X'"Informações de Status CT-e / Autorizado
     WITH ck_canc  EQ '' "Informações de Status CT-e / Cancelado
     WITH ck_ambos EQ '' "Informações de Status CT-e / Ambos
     WITH cg_t1 EQ 'X' "Check informação de chegada de documento / Todos
     WITH cg_t2 EQ ''  "Check informação de chegada de documento / Pendente
     WITH cg_t3 EQ ''  "Check informação de chegada de documento / Confirmado
     WITH cg_t4 EQ ''  "Check informação de chegada de documento / Ativa Check
     WITH cc_t1 EQ ''  "Check informação de liberação pagamento / Todos
     WITH cc_t2 EQ 'X' "Check informação de liberação pagamento / Pendente
     WITH cc_t3 EQ ''  "Check informação de liberação pagamento / Autorizado
     WITH cc_t4 EQ ''  "Check informação de liberação pagamento / Bloqueado
     WITH ck_t1 EQ ''  "Check informação de bloqueio pagamento / Todos
     WITH ck_t2 EQ ''  "Check informação de bloqueio pagamento / Bloqueado
     WITH ck_t3 EQ 'X'  "Check informação de bloqueio pagamento / Desbloqueado
     WITH pproc  IN pproc "Tipo de proc.pagamento frete
     WITH pmodal IN pmodal "Modal
     WITH etomad IN etomad
     WITH dtemit IN dtemit
     WITH chavec IN chavec
     AND RETURN.
    PERFORM f_get_runtime_info.

*&-------------------------Proc. Principal-------------------------------*
    IF <lt_data> IS ASSIGNED.
      FREE: it_alv_saida.
      MOVE-CORRESPONDING <lt_data> TO it_alv_saida.

      IF it_alv_saida IS NOT INITIAL.
*            selecionando dados fornecedor.
        FREE: it_forn.
        SELECT * FROM lfa1 AS a
        INNER JOIN lfb1 AS b ON b~lifnr EQ a~lifnr
        INNER JOIN t052 AS c ON c~zterm EQ b~zterm
        INTO CORRESPONDING FIELDS OF TABLE it_forn
          FOR ALL ENTRIES IN it_alv_saida
        WHERE a~lifnr EQ it_alv_saida-p_emissor
          AND b~bukrs EQ it_alv_saida-e_tomadora.
      ENDIF.

      LOOP AT <lt_data> ASSIGNING <ls_data>.
        CLEAR: wa_entrada.
        MOVE-CORRESPONDING <ls_data> TO wa_entrada.

        FREE:it_n55_t.
        "Busca Notas Fiscais Modelo 55
        CALL METHOD zcl_cte_dist_g=>busca_notas_n55
          EXPORTING
            p_chave_cte = wa_entrada-cd_chave_cte
          IMPORTING
            e_n55_t     = it_n55_t.

        CLEAR:wa_n55_t.
        READ TABLE it_n55_t INTO wa_n55_t INDEX 1.
        CHECK sy-subrc IS INITIAL AND wa_n55_t-tknum IS INITIAL.

*&-----------------------------------------------------------------------*
* Reiniciar_pagamento / fazer a mesma validação do
* P. reiniciar_pagamentos e utilizar a mesmo method existente
* "ler_dados_xi".
        obj_cte = NEW #( ). " Create Object

        CALL METHOD obj_cte->ler_dados_xi
          EXPORTING
            p_chave_cte  = wa_entrada-cd_chave_cte
          EXCEPTIONS
            foreign_lock = 1
            OTHERS       = 2.

*&-----------------------------------------------------------------------*
* Gerar VT/VI / fazer a mesma validação do
* P. gerar_documento_transporte e utlizar o mesmo method existem
* "gerar_doc_transporte".
        IF sy-subrc IS INITIAL.

          IF wa_entrada-zbvtyp IS NOT INITIAL.
            FREE: wa_info_forne.

            SELECT SINGLE * INTO zib_cte_dist_ter
              FROM zib_cte_dist_ter
             WHERE cd_chave_cte EQ wa_entrada-cd_chave_cte.

            CALL METHOD zcl_cte_dist_g=>busca_banco_parceiro
              IMPORTING
                e_lfbk     = e_lfbk
                e_bnka     = e_bnka
              CHANGING
                p_cte      = zib_cte_dist_ter
              EXCEPTIONS
                erro_banco = 1
                OTHERS     = 2.

            IF sy-subrc IS INITIAL.
              wa_info_forne-bvtyp = e_lfbk-bvtyp.
              wa_info_forne-bankl = e_bnka-bankl(3).
              wa_info_forne-banka = e_bnka-banka.
              wa_info_forne-bankn = e_lfbk-bankn.

              wa_cte_select = CORRESPONDING #( zib_cte_dist_ter ).

              IF NOT e_lfbk-bkont IS INITIAL.
                CONCATENATE e_lfbk-bankl+4(11) '-' e_lfbk-bkont INTO wa_info_forne-agenc.
              ELSE.
                wa_info_forne-agenc = e_lfbk-bankl+4(11).
              ENDIF.
            ENDIF.
          ELSE.
            CLEAR: wa_info_forne.

            SELECT SINGLE * INTO zib_cte_dist_ter
            FROM zib_cte_dist_ter
            WHERE cd_chave_cte EQ wa_entrada-cd_chave_cte.

            wa_cte_select = CORRESPONDING #( zib_cte_dist_ter ).
          ENDIF.

          CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
            EXPORTING
              chave          = wa_entrada-cd_chave_cte
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc IS NOT INITIAL.
            CALL METHOD zcl_cte_dist_g=>add_log_cte_job
              EXPORTING
                p_cd_chave_cte = wa_entrada-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_seq.
            CONTINUE.
          ENDIF.

          TRY .

              BREAK-POINT. "178025 CS2023000574 Job dinâmico PSA

              CALL METHOD obj_cte->gerar_doc_transporte
                EXPORTING
                  p_cte_chave = wa_entrada-cd_chave_cte
                EXCEPTIONS
                  doc_transp  = 1
                  OTHERS      = 2.

              IF sy-subrc IS NOT INITIAL.
                "Se não gerou a VT/VI não seguir para os proximos processos, retornar para proximo documento da fila.
                CALL METHOD zcl_cte_dist_g=>add_log_cte_job
                  EXPORTING
                    p_cd_chave_cte = wa_entrada-cd_chave_cte
                    p_type         = 'E'
                    p_id           = sy-msgid
                    p_num          = sy-msgno
                    p_message_v1   = 'Erro ao gerar VT/VI'
                    p_message_v2   = sy-msgv2
                    p_message_v3   = sy-msgv3
                    p_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_seq.

                CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
                  EXPORTING
                    chave = wa_entrada-cd_chave_cte.

                CONTINUE.
              ENDIF.

            CATCH zcx_cte_dist_g INTO DATA(ex_erro)..
              CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
                EXPORTING
                  chave = wa_entrada-cd_chave_cte.

              CONTINUE.
          ENDTRY.

          "Time para reinicializar os documentos.
          WAIT UP TO 05 SECONDS.

*&-----------------------------------------------------------------------*
* Reiniciar_pagamento novamente apos geração da VT/VI
* "ler_dados_xi".
*              obj_cte = NEW #( ). " Create Object
          CALL METHOD obj_cte->ler_dados_xi
            EXPORTING
              p_chave_cte  = wa_entrada-cd_chave_cte
            EXCEPTIONS
              foreign_lock = 1
              OTHERS       = 2.


          "Verifica a data de vencimento.
          CLEAR: ws_forn, vg_dias.
          READ TABLE it_forn INTO ws_forn WITH KEY lifnr = wa_entrada-p_emissor.
          IF ws_forn-zterm IS NOT INITIAL.

            CLEAR: zcond_pto, zvencimento, zdatevenc.
            zcond_pto = ws_forn-zterm(1).
            CONDENSE zcond_pto NO-GAPS.

            CASE zcond_pto.
              WHEN 'Z'.
                zvencimento = ws_forn-ztag1.
                wa_entrada-zdt_vencto   = wa_entrada-dt_emissao + zvencimento.
              WHEN OTHERS.
                wa_entrada-zdt_mov = sy-datum.
                wa_entrada-zdt_vencto = sy-datum.
            ENDCASE.
          ELSE.

            CALL METHOD zcl_cte_dist_g=>add_log_cte_job
              EXPORTING
                p_cd_chave_cte = wa_entrada-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = 'Condição de pagamento, '
                p_message_v2   = 'não cadastrada para o fornecedor'
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_seq.
            CONTINUE.
          ENDIF.

          DATA(i_data_vencimento) = wa_entrada-zdt_vencto.

          CALL METHOD obj_cte->busca_proximo_venc_fatura
            IMPORTING
              e_data_vencimento = wa_entrada-zdt_vencto
            EXCEPTIONS
              erro              = 1
              OTHERS            = 2.

          IF i_data_vencimento > wa_entrada-zdt_vencto.
            wa_entrada-zdt_vencto = i_data_vencimento.
          ENDIF.

          zcl_miro=>get_proximo_dia_util( EXPORTING i_data_base = wa_entrada-zdt_vencto
                                                    i_signum    = '+'
                                          RECEIVING r_data      = DATA(r_data) ).

          "Ajuste realizado na data de vencimento / AOENNING / 20.04.2023.
          wa_entrada-zdt_vencto = r_data.
          "Ajuste realizado na data de vencimento / AOENNING / 20.04.2023.
*              ENDDO.

          "Alterar a data de vencimento.
          CLEAR: wa_cte_dist_ter.
          SELECT SINGLE * INTO wa_cte_dist_ter
          FROM zib_cte_dist_ter
          WHERE cd_chave_cte EQ wa_entrada-cd_chave_cte.

          wa_cte_dist_ter-zdt_mov = sy-datum.
          wa_cte_dist_ter-zdt_vencto = wa_entrada-zdt_vencto.

          MODIFY zib_cte_dist_ter FROM wa_cte_dist_ter.
          COMMIT WORK.
          WAIT UP TO 02 SECONDS.


*&-----------------------------------------------------------------------*
* Reiniciar_pagamento novamente apos geração da VT/VI
* "ler_dados_xi".
*              obj_cte = NEW #( ). " Create Object
          CALL METHOD obj_cte->ler_dados_xi
            EXPORTING
              p_chave_cte  = wa_entrada-cd_chave_cte
            EXCEPTIONS
              foreign_lock = 1
              OTHERS       = 2.



*&-----------------------------------------------------------------------*
* SALVAR_INFORMACOES_DOC PARA GERAR PAGAMENTO.
*          PERFORM preencher_informacoes_doc USING wa_cte_dist_ter.



*&-----------------------------------------------------------------------*
* SALVAR_INFORMACOES_DOC PARA GERAR PAGAMENTO.
          CALL FUNCTION 'ZCTE_DIST_FATURAMENTO_FERROV'
            EXPORTING
              i_cte = wa_cte_dist_ter.
          WAIT UP TO 02 SECONDS.


          CLEAR: wa_cte_dist_ter.
          SELECT SINGLE * INTO wa_cte_dist_ter
          FROM zib_cte_dist_ter
          WHERE cd_chave_cte EQ wa_entrada-cd_chave_cte.


*&-----------------------------------------------------------------------*
* Gerar pagamento / fazer a mesma validação do
* P. gerar_pagamentos USING abap_false e utlizar a o
* mesmo method da classe  ja existente "gerar_fatura_frete".

          CALL FUNCTION 'ZENQUEUE_CTE_TERCEIRO'
            EXPORTING
              chave          = wa_entrada-cd_chave_cte
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc IS NOT INITIAL.
            CALL METHOD zcl_cte_dist_g=>add_log_cte_job
              EXPORTING
                p_cd_chave_cte = wa_entrada-cd_chave_cte
                p_type         = sy-msgty
                p_id           = sy-msgid
                p_num          = sy-msgno
                p_message_v1   = sy-msgv1
                p_message_v2   = sy-msgv2
                p_message_v3   = sy-msgv3
                p_message_v4   = sy-msgv4
              CHANGING
                p_lc_sequencia = lc_seq.
            CONTINUE.
          ENDIF.

          it_chaves_bloq-cd_chave_cte = wa_cte_select-cd_chave_cte.
          APPEND it_chaves_bloq.

          TRY .
              CLEAR: ex_erro.
              CALL METHOD obj_cte->gerar_fatura_frete
                EXPORTING
                  p_chave_cte         = wa_cte_dist_ter-cd_chave_cte
                  p_estornar          = lv_estornar
                CHANGING
                  p_cte               = wa_cte_dist_ter
                EXCEPTIONS
                  nao_enc_frete       = 1
                  fatura              = 2
                  pedido              = 3
                  cod_iva             = 4
                  banco_parceiro      = 5
                  param_ctb           = 6
                  banco_empresa       = 7
                  sem_vt              = 8
                  erro_entrada_fiscal = 9
                  miro_compensada     = 11
                  peso_chegada        = 12
                  OTHERS              = 13.

              IF sy-subrc IS NOT INITIAL.
                CALL METHOD zcl_cte_dist_g=>add_log_cte_job
                  EXPORTING
                    p_cd_chave_cte = wa_entrada-cd_chave_cte
                    p_type         = sy-msgty
                    p_id           = sy-msgid
                    p_num          = sy-msgno
                    p_message_v1   = sy-msgv1
                    p_message_v2   = sy-msgv2
                    p_message_v3   = sy-msgv3
                    p_message_v4   = sy-msgv4
                  CHANGING
                    p_lc_sequencia = lc_seq.
              ENDIF.

            CATCH zcx_miro_exception INTO DATA(ex_miro).
              CALL METHOD zcl_cte_dist_g=>add_log_cte_job
                EXPORTING
                  p_cd_chave_cte = wa_entrada-cd_chave_cte
                  p_type         = sy-msgty
                  p_id           = sy-msgid
                  p_num          = sy-msgno
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_seq.

            CATCH zcx_cte_dist_g INTO ex_erro.
              CALL METHOD zcl_cte_dist_g=>add_log_cte_job
                EXPORTING
                  p_cd_chave_cte = wa_entrada-cd_chave_cte
                  p_type         = sy-msgty
                  p_id           = sy-msgid
                  p_num          = sy-msgno
                  p_message_v1   = sy-msgv1
                  p_message_v2   = sy-msgv2
                  p_message_v3   = sy-msgv3
                  p_message_v4   = sy-msgv4
                CHANGING
                  p_lc_sequencia = lc_seq.
          ENDTRY.

          CALL FUNCTION 'ZDENQUEUE_CTE_TERCEIRO'
            EXPORTING
              chave = wa_entrada-cd_chave_cte.

          DELETE it_chaves_bloq WHERE cd_chave_cte EQ wa_cte_select-cd_chave_cte.

        ELSE.

          CALL METHOD zcl_cte_dist_g=>add_log_cte_job
            EXPORTING
              p_cd_chave_cte = wa_entrada-cd_chave_cte
              p_type         = 'E'
              p_id           = sy-msgid
              p_num          = sy-msgno
              p_message_v1   = sy-msgv1
              p_message_v2   = sy-msgv2
              p_message_v3   = sy-msgv3
              p_message_v4   = sy-msgv4
            CHANGING
              p_lc_sequencia = lc_seq.

        ENDIF.

        APPEND wa_entrada TO it_entrada.
        CLEAR: wa_entrada, wa_info_forne, wa_cte_select, r_data, i_data_vencimento.
        FREE: it_cte[].
      ENDLOOP.


      "Fim do processamento.
      IF <lt_data> IS NOT INITIAL AND sy-batch IS INITIAL.
        MESSAGE 'Processo executado com sucesso!'(001) TYPE 'I' DISPLAY LIKE 'E'.
      ELSEIF <lt_data> IS INITIAL AND sy-batch IS INITIAL.
        MESSAGE 'Informações não encontradas para o processo!'(002) TYPE 'I' DISPLAY LIKE 'E'.

      ELSEIF sy-batch IS NOT INITIAL.
        CALL METHOD zcl_cte_dist_g=>add_log_cte_job
          EXPORTING
            p_cd_chave_cte = wa_entrada-cd_chave_cte
            p_type         = 'S'
            p_id           = ''
            p_num          = 01
            p_message_v1   = 'JOB processado com sucesso'
            p_message_v2   = ''
            p_message_v3   = ''
            p_message_v4   = ''
          CHANGING
            p_lc_sequencia = lc_seq.

      ENDIF.
    ENDIF.

  ELSE.

    CALL METHOD zcl_cte_dist_g=>add_log_cte_job
      EXPORTING
        p_cd_chave_cte = '' "'wa_entrada-cd_chave_cte'
        p_type         = 'E'
        p_id           = sy-msgid
        p_num          = sy-msgno
        p_message_v1   = 'Falta autorização para gerar VT/VI'
        p_message_v2   = 'ZACTFTTER_AUTH'
        p_message_v3   = ''
        p_message_v4   = ''
      CHANGING
        p_lc_sequencia = lc_seq.

  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_INFORMACOES_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_informacoes_doc USING p_chav TYPE zde_chave_doc_e.
  FIELD-SYMBOLS: <n55> TYPE zib_cte_dist_n55,
                 <n01> TYPE zib_cte_dist_n01,
                 <nit> TYPE zib_cte_dist_nit.

  DATA: it_0301_dup TYPE TABLE OF zib_cte_dist_dup    WITH HEADER LINE,
        it_0301_n55 TYPE TABLE OF zib_cte_dist_n55    WITH HEADER LINE,
        it_0301_n01 TYPE TABLE OF zib_cte_dist_n01    WITH HEADER LINE,
        it_0301_nit TYPE TABLE OF zib_cte_dist_nit    WITH HEADER LINE,
        it_0301_vt  TYPE TABLE OF zde_cte_dist_vt_alv WITH HEADER LINE.


  CLEAR: wa_cte_dist_ter,it_0301_dup,it_0301_n55,it_0301_n01,it_0301_nit,it_0301_vt ,it_0301_dup[],it_0301_n55[],it_0301_n01[],it_0301_nit[],it_0301_vt[].

  SELECT SINGLE * INTO wa_cte_dist_ter
  FROM zib_cte_dist_ter
  WHERE cd_chave_cte EQ p_chav.


  CHECK wa_cte_dist_ter IS NOT INITIAL.

*  0  ct-e normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto
  CASE wa_cte_dist_ter-cd_tipo_cte.
    WHEN 0 OR 3. "0  CT-e Normal/CT-e Substituto

      CLEAR: it_0301_n55[], it_0301_n01[], it_0301_nit[].

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ wa_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ wa_cte_dist_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      wa_cte_dist_ter-zvlr_vi         = 0.
      wa_cte_dist_ter-zvlr_frete      = 0.
      wa_cte_dist_ter-zvlr_mercadoria = 0.
      wa_cte_dist_ter-peso_origem     = 0.
      wa_cte_dist_ter-peso_chegada    = 0.
      wa_cte_dist_ter-zpeso_diferenca = 0.
      wa_cte_dist_ter-zquebra         = 0.
      wa_cte_dist_ter-zperda          = 0.
      wa_cte_dist_ter-zvlr_quebra     = 0.
      wa_cte_dist_ter-zvlr_perda      = 0.
      wa_cte_dist_ter-zvlr_liq_pagar  = 0.
      wa_cte_dist_ter-ck_peso_chegada = abap_true.

*      LOOP AT it_0301_vt.
*        ADD it_0301_vt-zvlr_vi         TO wa_cte_dist_ter-zvlr_vi.
*        ADD it_0301_vt-zvlr_frete      TO wa_cte_dist_ter-zvlr_frete.
*        ADD it_0301_vt-zvlr_mercadoria TO wa_cte_dist_ter-zvlr_mercadoria.
*        ADD it_0301_vt-zpeso_diferenca TO wa_cte_dist_ter-zpeso_diferenca.
*        ADD it_0301_vt-zquebra         TO wa_cte_dist_ter-zquebra.
*        ADD it_0301_vt-zperda          TO wa_cte_dist_ter-zperda.
*        ADD it_0301_vt-zvlr_quebra     TO wa_cte_dist_ter-zvlr_quebra.
*        ADD it_0301_vt-zvlr_perda      TO wa_cte_dist_ter-zvlr_perda.
*        ADD it_0301_vt-zvlr_liq_pagar  TO wa_cte_dist_ter-zvlr_liq_pagar.
*        ADD it_0301_vt-peso_chegada    TO wa_cte_dist_ter-peso_chegada.
*
*        IF it_0301_vt-ck_autorizado EQ abap_false.
*          ADD it_0301_vt-peso_origem  TO wa_cte_dist_ter-peso_origem.
*        ENDIF.

      "Ajustando valores notas 55
*        LOOP AT it_0301_n55 ASSIGNING <n55> WHERE tknum EQ it_0301_vt-tknum.
*          <n55>-zvlr_frete       = it_0301_vt-zvlr_frete.
*          <n55>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
*          <n55>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
*          <n55>-zvlr_perda       = it_0301_vt-zvlr_perda.
*          <n55>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
*          <n55>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n55>-docnum_nfe.
*            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
*            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
*            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
*
*            IF it_0301_vt-ck_autorizado EQ abap_false.
*              <nit>-peso_origem    = it_0301_vt-peso_origem.
*              <nit>-peso_chegada   = it_0301_vt-peso_chegada.
*            ELSE.
*              ADD <nit>-peso_origem TO zib_cte_dist_ter-peso_origem.
*            ENDIF.
*
*            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
*            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
*            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
*            <nit>-zquebra          = it_0301_vt-zquebra.
*            <nit>-zperda           = it_0301_vt-zperda.
*            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
*            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
*            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
*            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
*            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
*            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              zib_cte_dist_ter-ck_peso_chegada = abap_false.
*            ENDIF.
*          ENDLOOP.
*        ENDLOOP.

      "Ajustando valores notas 01
*        LOOP AT it_0301_n01 ASSIGNING <n01> WHERE tknum EQ it_0301_vt-tknum.
*          <n01>-zvlr_frete       = it_0301_vt-zvlr_frete.
*          <n01>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
*          <n01>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
*          <n01>-zvlr_perda       = it_0301_vt-zvlr_perda.
*          <n01>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar .
*          LOOP AT it_0301_nit ASSIGNING <nit> WHERE docnum EQ <n01>-docnum_nf.
*            <nit>-zvlr_vi          = it_0301_vt-zvlr_vi.
*            <nit>-zvlr_frete       = it_0301_vt-zvlr_frete.
*            <nit>-zvlr_mercadoria  = it_0301_vt-zvlr_mercadoria.
*            <nit>-peso_origem      = it_0301_vt-peso_origem.
*            <nit>-peso_chegada     = it_0301_vt-peso_chegada.
*            <nit>-zpeso_diferenca  = it_0301_vt-zpeso_diferenca.
*            <nit>-zvlr_kg_transp   = it_0301_vt-zvlr_kg_transp.
*            <nit>-zvlr_kg_mercad   = it_0301_vt-zvlr_kg_mercad.
*            <nit>-zquebra          = it_0301_vt-zquebra.
*            <nit>-zperda           = it_0301_vt-zperda.
*            <nit>-zvlr_quebra      = it_0301_vt-zvlr_quebra.
*            <nit>-zvlr_perda       = it_0301_vt-zvlr_perda.
*            <nit>-zvlr_liq_pagar   = it_0301_vt-zvlr_liq_pagar.
*            <nit>-pc_quebra        = it_0301_vt-pc_quebra.
*            <nit>-pc_tolerancia    = it_0301_vt-pc_tolerancia.
*            <nit>-ck_peso_digitado = it_0301_vt-ck_peso_digitado.
*            IF it_0301_vt-ck_peso_digitado EQ abap_false.
*              wa_cte_dist_ter-ck_peso_chegada = abap_false.
*            ENDIF.
*          ENDLOOP.
*        ENDLOOP.
*      ENDLOOP.

      wa_cte_dist_ter-zbase_icms    = 0.
      wa_cte_dist_ter-zbase_pis     = 0.
      wa_cte_dist_ter-zbase_cofins  = 0.
      wa_cte_dist_ter-zrate_icms    = 0.
      wa_cte_dist_ter-zrate_pis     = 0.
      wa_cte_dist_ter-zrate_cofins  = 0.
      wa_cte_dist_ter-zvalor_icms   = 0.
      wa_cte_dist_ter-zvalor_pis    = 0.
      wa_cte_dist_ter-zvalor_cofins = 0.

*      IF it_0301_n55[] IS NOT INITIAL.
*        MODIFY zib_cte_dist_n55 FROM TABLE it_0301_n55.
*      ENDIF.
*
*      IF it_0301_n01[] IS NOT INITIAL.
*        MODIFY zib_cte_dist_n01 FROM TABLE it_0301_n01.
*      ENDIF.
*
*      IF it_0301_nit[] IS NOT INITIAL.
*        MODIFY zib_cte_dist_nit FROM TABLE it_0301_nit.
*      ENDIF.

    WHEN 1. "1  CT-e de Complemento de Valores
      wa_cte_dist_ter-ck_peso_chegada = abap_true.
  ENDCASE.


  "Buscar Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE l~matnr INTO @DATA(lc_matnr)
    FROM j_1bnflin AS l
   INNER JOIN zib_cte_dist_n55 AS n ON n~docnum_nfe EQ l~docnum
   WHERE n~cd_chave_cte EQ @wa_cte_dist_ter-cd_chave_cte
     AND n~docnum_nfe   NE @space.

  IF sy-subrc IS NOT INITIAL.
    SELECT SINGLE l~matnr INTO lc_matnr
      FROM j_1bnflin AS l
     INNER JOIN zib_cte_dist_n01 AS n ON n~docnum_nf EQ l~docnum
     WHERE n~cd_chave_cte EQ wa_cte_dist_ter-cd_chave_cte
       AND n~docnum_nf    NE space.
  ENDIF.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  CALL METHOD zcl_cte_dist_g=>busca_impostos_taxas
    EXPORTING
      p_iva            = wa_cte_dist_ter-mwskz
      p_data_documento = wa_cte_dist_ter-dt_emissao
      p_shipfrom       = wa_cte_dist_ter-inicio_uf
      p_shipto         = wa_cte_dist_ter-termino_uf
      e_tomadora       = wa_cte_dist_ter-e_tomadora
      f_tomadora       = wa_cte_dist_ter-f_tomadora
      p_emissora       = wa_cte_dist_ter-p_emissor
      p_matnr          = lc_matnr
    IMPORTING
      e_rate_icms      = wa_cte_dist_ter-zrate_icms
      e_rate_pis       = wa_cte_dist_ter-zrate_pis
      e_rate_cofins    = wa_cte_dist_ter-zrate_cofins
    EXCEPTIONS
      sem_iva          = 1
      OTHERS           = 2.

  IF sy-subrc IS INITIAL.
    IF wa_cte_dist_ter-zrate_icms GT 0.
      wa_cte_dist_ter-zbase_icms  = wa_cte_dist_ter-zvlr_frete.
      wa_cte_dist_ter-zvalor_icms = wa_cte_dist_ter-zvlr_frete * ( wa_cte_dist_ter-zrate_icms / 100 ).
    ELSE.
      wa_cte_dist_ter-zbase_icms  = 0.
      wa_cte_dist_ter-zvalor_icms = 0.
    ENDIF.

    DATA(lva_base_calc_pis_cofins) = zcl_cte_dist_g=>get_base_pis_cofins( i_valor_frete = CONV #( wa_cte_dist_ter-zvlr_frete )
                                                                          i_valor_icms  = CONV #( wa_cte_dist_ter-zvalor_icms ) ).

    IF wa_cte_dist_ter-zrate_pis GT 0.
      wa_cte_dist_ter-zbase_pis  = lva_base_calc_pis_cofins.
      wa_cte_dist_ter-zvalor_pis = lva_base_calc_pis_cofins * ( wa_cte_dist_ter-zrate_pis / 100 ).
    ELSE.
      wa_cte_dist_ter-zbase_pis  = 0.
      wa_cte_dist_ter-zvalor_pis = 0.
    ENDIF.


    IF wa_cte_dist_ter-zrate_cofins GT 0.
      wa_cte_dist_ter-zbase_cofins  = lva_base_calc_pis_cofins.
      wa_cte_dist_ter-zvalor_cofins = lva_base_calc_pis_cofins * ( wa_cte_dist_ter-zrate_cofins / 100 ).
    ELSE.
      wa_cte_dist_ter-zbase_cofins  = 0.
      wa_cte_dist_ter-zvalor_cofins = 0.
    ENDIF.
  ENDIF.

  MODIFY zib_cte_dist_ter FROM wa_cte_dist_ter.
  COMMIT WORK.
  WAIT UP TO 02 SECONDS.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_INFORMACOES_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ENTRADA_CD_CHAVE_CTE  text
*----------------------------------------------------------------------*
FORM preencher_informacoes_doc  USING p_cte_ter TYPE zib_cte_dist_ter.

  CLEAR: it_0301_vt[], it_0301_n55[], it_0301_n01[], it_0301_nit[].

  FIELD-SYMBOLS: <fs_0301_vt> TYPE zde_cte_dist_vt_alv.

  wa_0301_ter = p_cte_ter.

  wa_0301_ter-zdt_vencto = p_cte_ter-zdt_vencto.

*  0  CT-e Normal
*  1  CT-e de Complemento de Valores
*  2  CT-e de Anulação de Valores
*  3  CT-e Substituto

  CASE p_cte_ter-cd_tipo_cte.
    WHEN 0 OR 3. "CT-e Normal/CT-e Substituta

      SELECT *
        INTO TABLE it_0301_n55
        FROM zib_cte_dist_n55
       WHERE cd_chave_cte EQ p_cte_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nfe   NE space.

      IF it_0301_n55[] IS NOT INITIAL.
        SELECT *
          INTO TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n55
         WHERE cd_chave_cte EQ it_0301_n55-cd_chave_cte
           AND docnum       EQ it_0301_n55-docnum_nfe.
      ENDIF.

      SELECT *
        INTO TABLE it_0301_n01
        FROM zib_cte_dist_n01
       WHERE cd_chave_cte EQ p_cte_ter-cd_chave_cte
         AND tknum        NE space
         AND docnum_nf    NE space.

      IF it_0301_n01[] IS NOT INITIAL.
        SELECT *
          APPENDING TABLE it_0301_nit
          FROM zib_cte_dist_nit
           FOR ALL ENTRIES IN it_0301_n01
         WHERE cd_chave_cte EQ it_0301_n01-cd_chave_cte
           AND docnum       EQ it_0301_n01-docnum_nf.
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
      DATA: lc_ck_autorizados TYPE c LENGTH 1,
            lc_matnr          TYPE matnr,
            lc_grupo          TYPE matkl,
            lc_tipo           TYPE zde_tp_aut_frete.

      lc_ck_autorizados = abap_true.

      LOOP AT it_0301_nit.

        SELECT SINGLE matnr INTO lc_matnr
          FROM j_1bnflin
         WHERE docnum EQ it_0301_nit-docnum
           AND itmnum EQ it_0301_nit-itmnum.

        SELECT SINGLE matkl INTO lc_grupo
          FROM mara
         WHERE matnr EQ lc_matnr.

        SELECT SINGLE tp_aut_frete
          INTO lc_tipo
          FROM zib_cte_dist_gm
         WHERE matkl EQ lc_grupo.

        IF ( sy-subrc IS INITIAL ) AND ( it_0301_nit-ck_autorizado NE abap_true ) AND ( p_cte_ter-cd_modal NE '04' ).
          lc_ck_autorizados = abap_false.
        ENDIF.
      ENDLOOP.
      "" Verfica Autorização de Pagamento """""""""""""""""""""""""""""""""""""""""""
      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      LOOP AT it_0301_n55.
        CLEAR: it_0301_vt.
        READ TABLE it_0301_nit WITH KEY docnum = it_0301_n55-docnum_nfe.
        it_0301_vt-tknum            = it_0301_n55-tknum.
        it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
        it_0301_vt-zvlr_vi          = it_0301_n55-zvlr_vi.
        it_0301_vt-zvlr_mercadoria  = it_0301_n55-zvlr_mercadoria.

        "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
        IF it_0301_nit-ck_autorizado EQ abap_true.
          it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
        ELSE.
          it_0301_vt-zvlr_frete	      = it_0301_n55-zvlr_frete.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
        ENDIF.

        it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
        it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
        it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
        it_0301_vt-zquebra          = it_0301_nit-zquebra.
        it_0301_vt-zperda           = it_0301_nit-zperda.
        it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
        it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
        it_0301_vt-zvlr_quebra      = it_0301_n55-zvlr_quebra.
        it_0301_vt-zvlr_perda	      = it_0301_n55-zvlr_perda.
        it_0301_vt-zvlr_liq_pagar	  = it_0301_n55-zvlr_liq_pagar.
        it_0301_vt-ck_peso_digitado = it_0301_n55-ck_peso_digitado.
        it_0301_vt-docnum           = it_0301_nit-docnum.
        it_0301_vt-itmnum           = it_0301_nit-itmnum.

        IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( p_cte_ter-ck_peso_chegada EQ abap_true ).
          it_0301_vt-ic_editar = icon_set_state.
        ELSE.
          it_0301_vt-ic_editar = icon_change_number.
        ENDIF.
        APPEND it_0301_vt.
      ENDLOOP.

      LOOP AT it_0301_n01.
        CLEAR: it_0301_vt.
        READ TABLE it_0301_nit WITH KEY docnum = it_0301_n01-docnum_nf.
        it_0301_vt-tknum            = it_0301_n01-tknum.
        it_0301_vt-zmatnr_merc      = it_0301_nit-zmatnr_merc.
        it_0301_vt-zvlr_vi          = it_0301_n01-zvlr_vi.
        it_0301_vt-zvlr_mercadoria  = it_0301_n01-zvlr_mercadoria.

        "Busca Peso Autorizado """""""""""""""""""""""""""""""""""""
        IF it_0301_nit-ck_autorizado EQ abap_true.
          it_0301_vt-zvlr_frete	      = it_0301_nit-zvlr_frete_apro.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem_apro.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada_apr.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-peso_difere_apro.
        ELSE.
          it_0301_vt-zvlr_frete	      = it_0301_n01-zvlr_frete.
          it_0301_vt-peso_origem      = it_0301_nit-peso_origem.
          it_0301_vt-peso_chegada	    = it_0301_nit-peso_chegada.
          it_0301_vt-zpeso_diferenca  = it_0301_nit-zpeso_diferenca.
        ENDIF.

        it_0301_vt-ck_autorizado    = it_0301_nit-ck_autorizado.
        it_0301_vt-zvlr_kg_transp	  = it_0301_nit-zvlr_kg_transp.
        it_0301_vt-zvlr_kg_mercad	  = it_0301_nit-zvlr_kg_mercad.
        it_0301_vt-zquebra          = it_0301_nit-zquebra.
        it_0301_vt-zperda           = it_0301_nit-zperda.
        it_0301_vt-pc_quebra        = it_0301_nit-pc_quebra.
        it_0301_vt-pc_tolerancia    = it_0301_nit-pc_tolerancia.
        it_0301_vt-zvlr_quebra      = it_0301_n01-zvlr_quebra.
        it_0301_vt-zvlr_perda	      = it_0301_n01-zvlr_perda.
        it_0301_vt-zvlr_liq_pagar	  = it_0301_n01-zvlr_liq_pagar.
        it_0301_vt-ck_peso_digitado = it_0301_n01-ck_peso_digitado.

        IF ( it_0301_vt-ck_peso_digitado EQ abap_true ) OR ( p_cte_ter-ck_peso_chegada EQ abap_true ).
          it_0301_vt-ic_editar = icon_set_state.
        ELSE.
          it_0301_vt-ic_editar = icon_change_number.
        ENDIF.

        APPEND it_0301_vt.
      ENDLOOP.

      "Conhecimento de Transporte - Modal: Ferroviário
      IF wa_0301_ter-cd_modal = '04'.
        IF wa_0301_ter-dt_chegada IS INITIAL.
          wa_0301_ter-dt_chegada = wa_0301_ter-dt_emissao.
        ENDIF.
        LOOP AT it_0301_vt ASSIGNING <fs_0301_vt>.
          IF <fs_0301_vt>-peso_chegada IS INITIAL.
            <fs_0301_vt>-peso_chegada = <fs_0301_vt>-peso_origem.
          ENDIF.
        ENDLOOP.
      ENDIF.

    WHEN 1. "CT-e de Complemento de Valores
      it_0301_vt-zvlr_vi          = 0.
      it_0301_vt-zvlr_frete       = p_cte_ter-zvlr_frete.
      it_0301_vt-zvlr_mercadoria  = 0.
      it_0301_vt-peso_origem      = 0.
      it_0301_vt-peso_chegada     = 0.
      it_0301_vt-zpeso_diferenca  = 0.
      it_0301_vt-zvlr_kg_transp   = 0.
      it_0301_vt-zvlr_kg_mercad   = 0.
      it_0301_vt-zquebra          = 0.
      it_0301_vt-zperda           = 0.
      it_0301_vt-zvlr_quebra      = 0.
      it_0301_vt-zvlr_perda       = 0.
      it_0301_vt-zvlr_liq_pagar   = p_cte_ter-zvlr_frete.
      it_0301_vt-pc_quebra        = 0.
      it_0301_vt-pc_tolerancia    = 0.
      APPEND it_0301_vt.
  ENDCASE.

ENDFORM.
