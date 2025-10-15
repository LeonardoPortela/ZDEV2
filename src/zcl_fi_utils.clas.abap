class ZCL_FI_UTILS definition
  public
  final
  create public .

public section.

  interfaces IF_STAB_CONSTANTS .

  constants:
    BEGIN OF LC_BANCO,
        ITAU TYPE C LENGTH 3 VALUE '341',
      END OF LC_BANCO .
  constants:
    BEGIN OF LC_BENEFICIARIO,
        AMAGGI TYPE C LENGTH 12 VALUE '293800527831',
      END OF LC_BENEFICIARIO .
  constants:
    BEGIN OF LC_USER,
        USER_API TYPE C LENGTH 15 VALUE 'Z_USER_API_F110',
      END OF LC_USER .

  class-methods RUN_API_ITAU
    importing
      !I_ITAU_BOLETO type ZDE_IN_ITAU_BOLETO optional
      !I_ITAU_COB type ZDE_IN_ITAU_COB optional
      !IS_PIX type CHAR1 optional .
  class-methods GET_BOLETOS_ITAU
    importing
      !I_REQUEST type ZDE_IN_ITAU_BOLETO
    exporting
      !E_RETORNO_ITAU type ZDE_RETORNO_ITAU .
  class-methods SET_BOLETOS_ITAU
    importing
      !I_RETORNO_ITAU type ZDE_RETORNO_ITAU
      !IS_PIX type CHAR1 optional .
  class-methods CHECK_BOLETO_IMPORTADO
    importing
      !I_RETORNO_ITAU type ZDE_RETORNO_ITAU
    changing
      !C_RETORNO_ITAU type ZDE_RETORNO_ITAU .
  class-methods UPLOAD_AQUIVO_JSON_FROM_SAP .
  class-methods DEPARA_STATUS
    importing
      !I_STATUS type STRING
      !I_BAIXA type STRING optional
      !IS_PIX type CHAR1 optional
    exporting
      !E_STATUS type ZFIT0058-COD_MOV .
  class-methods SEND_PIX_ITAU
    importing
      !I_REGUP type REGUP
      !I_T012K type T012K
      !I_REGUH type REGUH
      !I_CNPJ type J_1BCGC optional .
  class-methods GET_COBRANCA_ITAU
    importing
      !I_REQUEST type ZDE_IN_ITAU_COB
    exporting
      !E_RETORNO_ITAU type ZDE_RETORNO_ITAU_GET_COB .
  class-methods CREATE_BOLECODE_ITAU
    importing
      !I_REQUEST type ZDE_IN_ITAU_BOLECODE
    exporting
      !E_RETORNO_BOLECODE_ITAU type ZDE_RETORNO_BOLECODE_ITAU .
  class-methods CHECK_BOLECODE_ITAU
    importing
      !I_NOSSO_NUMERO type ZDE_NOSSO_NUMERO optional
      !I_SOLICITACAO type ZSDED013
      !I_EMPRESA type VKORG optional
      !I_HBKID type BSID-HBKID
      !I_BELNR type BSID-BELNR optional
      !I_GJAHR type BSID-GJAHR optional
      !I_TIPO type CHAR1 optional
    changing
      !C_DADOS_BOLETO type ZFI_BOLETO optional
    returning
      value(E_RETORNO_ITAU) type ZDE_RETORNO_ITAU .
  class-methods GET_NOSSO_NUMERO
    returning
      value(R_NOSSO_NUMERO) type ZDE_NOSSO_NUMERO .
  class-methods UPDATE_INF_SMART_BOLETOS
    importing
      !I_RETORNO_ITAU type ZDE_RETORNO_ITAU
    changing
      !U_BOLETO type ZFI_BOLETO .
  class-methods FILL_BOLECODE
    importing
      !I_SMART_BOLETO type ZFI_BOLETO
    returning
      value(R_BOLECODE) type ZDE_IN_ITAU_BOLECODE .
  class-methods SET_BOLECODE_ITAU
    importing
      !I_BOLECODE type ZDE_RETORNO_BOLECODE_ITAU optional
      !I_SOLICITACAO type ZSDED013 optional .
  class-methods GET_BOLECODE_ITAU
    importing
      !I_SOLICITACAO type ZSDED013
    returning
      value(R_ZFIT0227) type ZFIT0227 .
  class-methods GET_ID_BENEFICIARIO
    returning
      value(R_ID_BENEFICIARIO) type ZDE_ID_BENEFICIARIO .
  class-methods SET_ID_BENEFICIARIO
    importing
      !I_EMPRESA type VKORG
      !I_HBKID type HBKID .
  class-methods CHECK_ACESSO
    importing
      !I_CNPJ type J_1BCGC
    returning
      value(IS_OK) type ABAP_BOOL .
  class-methods GET_STVARV
    importing
      !I_LOW type RVARI_VAL_255
      !I_NAME type RVARI_VNAM
    returning
      value(IS_OK) type ABAP_BOOL .
  class-methods SEND_PIX_BRADESCO
    importing
      !I_REGUP type REGUP
      !I_T012K type T012K
      !I_REGUH type REGUH
      !I_CNPJ type J_1BCGC optional .
  class-methods DOCUMENT_CHANGE
    importing
      !I_SOLICITACAO type ZSDED013
      !I_DTAID type DTAID_045T
      !I_EMPRESA type VKORG
      !I_HBKID type BSID-HBKID
      !I_BELNR type BSID-BELNR
      !I_GJAHR type BSID-GJAHR
      !I_TIPO type CHAR1
    returning
      value(R_ERRO) type CHAR1 .
  PROTECTED SECTION.

private section.

  class-data AT_ID_BENEFICIARIO type ZDE_ID_BENEFICIARIO .
ENDCLASS.



CLASS ZCL_FI_UTILS IMPLEMENTATION.


  METHOD GET_BOLETOS_ITAU.

    DATA: LV_RETRY TYPE CHAR1.
    DATA: LV_CONT  TYPE I.

    LV_CONT = 0.

    DO.

      IF LV_CONT >= 2.
        EXIT.
      ENDIF.

      ADD 1 TO LV_CONT.
      LV_RETRY = ABAP_FALSE.

      TRY.

          ZCL_INT_OB_FI_ITAU_BOLETOS=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE( )->EXECUTE_REQUEST(
            EXPORTING
              I_INFO_REQUEST         = I_REQUEST
            IMPORTING
            E_ID_INTEGRACAO          = DATA(RESUL_ID)
            E_INTEGRACAO             = DATA(RESULT_JSON)
          ).

          EXIT.

        CATCH ZCX_INTEGRACAO INTO DATA(LA_ZCX_INTEGRACAO).

          LV_RETRY = ABAP_TRUE.
          WAIT UP TO 1 SECONDS.

        CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).

          LV_RETRY = ABAP_TRUE.
          WAIT UP TO 1 SECONDS.

      ENDTRY.

    ENDDO.

    IF LV_RETRY IS NOT INITIAL.

      MESSAGE ID ZCX_ERROR->MSGID
              TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
              NUMBER ZCX_ERROR->MSGNO
                WITH ZCX_ERROR->MSGV1
                     ZCX_ERROR->MSGV2
                     ZCX_ERROR->MSGV3
                     ZCX_ERROR->MSGV4.
      RETURN.

    ENDIF.

*"// Recupera os dados do JSON
    IF RESULT_JSON IS NOT INITIAL.

      REPLACE 'numero_cadastro_nacional_pessoa_juridica'  IN RESULT_JSON-DS_DATA_RETORNO WITH 'cnpj_cpf'.
      REPLACE 'descricao_instrumento_cobranca'            IN RESULT_JSON-DS_DATA_RETORNO WITH 'des_ins_cob'.
      REPLACE 'numero_cadastro_pessoa_fisica'             IN RESULT_JSON-DS_DATA_RETORNO WITH 'cnpj_cpf'.
      REPLACE 'codigo_instituicao_financeira_pagamento'   IN RESULT_JSON-DS_DATA_RETORNO WITH 'cod_ins_fin_pag'.
      REPLACE 'comandar_instrucao_alterar_dados_cobranca' IN RESULT_JSON-DS_DATA_RETORNO WITH 'com_ins_alt_dados_cob'.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = RESULT_JSON-DS_DATA_RETORNO
        CHANGING
          DATA = E_RETORNO_ITAU.

      E_RETORNO_ITAU-ID_REFERENCIA  = RESUL_ID.

    ENDIF.

  ENDMETHOD.


  METHOD RUN_API_ITAU.

* "// Estrutura da API de Boleto
    IF I_ITAU_BOLETO IS NOT INITIAL.

* "// Chama a API de Boleto
      CALL METHOD GET_BOLETOS_ITAU
        EXPORTING
          I_REQUEST      = I_ITAU_BOLETO
        IMPORTING
          E_RETORNO_ITAU = DATA(E_RESPONSE_BOLETO).

* "// Armazena os dados na Tabelas
      CALL METHOD SET_BOLETOS_ITAU
        EXPORTING
          I_RETORNO_ITAU = E_RESPONSE_BOLETO
          IS_PIX         = IS_PIX.

    ENDIF.

    IF I_ITAU_COB IS NOT INITIAL.

* "// Chama a API de Cobrança
      CALL METHOD GET_COBRANCA_ITAU
        EXPORTING
          I_REQUEST      = I_ITAU_COB
        IMPORTING
          E_RETORNO_ITAU = DATA(E_RESPONSE_COBRANCA).

    ENDIF.


  ENDMETHOD.


  METHOD CHECK_BOLETO_IMPORTADO.

    TYPES: BEGIN OF TY_BOLETO,
             ID_BOLETO             TYPE ZFIT0054-SEQ2,
             SITUACAO_GERAL_BOLETO TYPE STRING,
             STATUS                TYPE STRING,
           END OF  TY_BOLETO.

    DATA: LT_BOLETO  TYPE TABLE OF TY_BOLETO,
          LV_COD_MOV TYPE ZFIT0058-COD_MOV.

    C_RETORNO_ITAU = I_RETORNO_ITAU.

    LOOP AT C_RETORNO_ITAU-DATA INTO DATA(LS_DADOS).

      LOOP AT LS_DADOS-DADO_BOLETO-DADOS_INDIVIDUAIS_BOLETO INTO DATA(LS_BOLETO).

        CALL METHOD DEPARA_STATUS
          EXPORTING
            I_STATUS = LS_BOLETO-SITUACAO_GERAL_BOLETO
          IMPORTING
            E_STATUS = LV_COD_MOV.

        APPEND VALUE #(
                        ID_BOLETO             = LS_DADOS-ID_BOLETO
                        SITUACAO_GERAL_BOLETO = LS_BOLETO-SITUACAO_GERAL_BOLETO
                        STATUS                = LV_COD_MOV
                      ) TO LT_BOLETO.
      ENDLOOP.

    ENDLOOP.

    SELECT *
      FROM ZFIT0054
        INTO TABLE @DATA(LT_ZFIT0054)
      FOR ALL ENTRIES IN @LT_BOLETO
        WHERE SEQ2 EQ @LT_BOLETO-ID_BOLETO.

    IF SY-SUBRC IS INITIAL.
      SELECT *
        FROM ZFIT0056
        INTO TABLE @DATA(LT_ZFIT0056)
        FOR ALL ENTRIES IN @LT_ZFIT0054
        WHERE COD_ARQ EQ @LT_ZFIT0054-COD_ARQ.
    ENDIF.

    LOOP AT LT_ZFIT0054 INTO DATA(LS_ZFIT0054).

      READ TABLE LT_BOLETO ASSIGNING FIELD-SYMBOL(<FS_CHECK>) WITH KEY ID_BOLETO = LS_ZFIT0054-SEQ2.
      IF SY-SUBRC IS INITIAL.

        READ TABLE LT_ZFIT0056 INTO DATA(LS_ZFIT0056) WITH KEY COD_ARQ = LS_ZFIT0054-COD_ARQ COD_RETORNO = <FS_CHECK>-STATUS.
        IF SY-SUBRC IS INITIAL.
          DELETE C_RETORNO_ITAU-DATA WHERE ID_BOLETO EQ <FS_CHECK>-ID_BOLETO.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD DEPARA_STATUS.

    DATA(U_STATUS) = I_STATUS.
    DATA(U_BAIXA) = I_BAIXA.

    IF IS_PIX IS NOT INITIAL.
      E_STATUS = '06'.
      RETURN.
    ENDIF.

    U_STATUS = |{ U_STATUS CASE = UPPER }|.

    E_STATUS = SWITCH #( U_STATUS
                WHEN 'EM ABERTO' THEN '02' "// EM ABERTO "ZFIT0058-02 - ENTRADA CONFIRMADA"
                WHEN 'PAGA'      THEN '06' "// PAGO      "ZFIT0058-06 - LIQUIDAÇÃO"
                WHEN 'BAIXADA'   THEN '02' "// EM ABERTO "ZFIT0058-02 - ENTRADA CONFIRMADA"
                ELSE '00' ).

    CHECK U_BAIXA IS NOT INITIAL.

    U_BAIXA = |{ U_BAIXA CASE = UPPER }|.

    CHECK U_STATUS EQ 'BAIXADA'.
    CHECK U_BAIXA  EQ 'BAIXA POR TER SIDO LIQUIDADO'.

    E_STATUS = '06'.

  ENDMETHOD.


  METHOD send_pix_itau.

    DATA: ls_in_itau_pix     TYPE zde_in_itau_pix,
          lv_cpf_cnpj        TYPE string,
          lv_conta           TYPE c LENGTH 8,
          lv_conta_recebedor TYPE c LENGTH 18,
          lv_qtd             TYPE c LENGTH 17,
          lv_bkref           TYPE lfbk-bkref,
          lv_cp              TYPE string.

    lv_cp = 'POUPAN'.

    SELECT SINGLE bankl
      FROM t012
      INTO @DATA(lv_bankl)
      WHERE hbkid EQ @i_t012k-hbkid
        AND bukrs EQ @i_t012k-bukrs.

    SELECT SINGLE lifnr, stkzn, stcd1, stcd2
      FROM lfa1
      INTO @DATA(ls_lfa1)
      WHERE lifnr EQ @i_regup-lifnr.

*** BUG - 192416  - 06.10.2025 - CBRAND - Inicio
    SELECT SINGLE businesspartner
      FROM ibupasuplrcotp
      INTO @DATA(lv_partner)
      WHERE supplier EQ @i_regup-lifnr.

*    SELECT SINGLE bankl
*      FROM but0bk
*      INTO @DATA(lv_codigo_banco)
*      WHERE partner EQ @i_regup-lifnr
*      AND bkvid EQ @i_regup-bvtyp.

    SELECT SINGLE bankl
      FROM but0bk
      INTO @DATA(lv_codigo_banco)
      WHERE partner EQ @lv_partner
      AND bkvid EQ @i_regup-bvtyp.
*** BUG - 192416  - 06.10.2025 - CBRAND - Fim

    SELECT SINGLE ispb_code
      FROM fiapbrd_ispb
      INTO @DATA(lv_ispb)
      WHERE bukrs EQ @i_regup-bukrs
      AND bank_key EQ @lv_codigo_banco(3).

    DATA(lv_f_j) = COND #( WHEN ls_lfa1-stkzn IS NOT INITIAL
                             THEN 'F'
                             ELSE 'J'
                          ).

    lv_cpf_cnpj = SWITCH #( lv_f_j
                              WHEN 'F' THEN ls_lfa1-stcd2
                              WHEN 'J' THEN ls_lfa1-stcd1
                          ).

    SELECT SINGLE belnr, gjahr, buzei, sgtxt
      FROM bseg
      INTO @DATA(ls_bseg)
      WHERE belnr EQ @i_regup-belnr
        AND gjahr EQ @i_regup-gjahr
        AND buzei EQ @i_regup-buzei.

    DATA(lv_max) = strlen( i_reguh-zbnkl  ).
    IF lv_max >= 4.
      DATA(lv_min) = lv_max - 4 .
    ENDIF.

    DATA(lv_max1) = strlen( lv_bankl ).
    IF lv_max1 >= 4.
      DATA(lv_min1) = lv_max1 - 4 .
    ENDIF.

    lv_conta = i_t012k-bankn.
    REPLACE ALL OCCURRENCES OF '-' IN lv_conta WITH ''.
    REPLACE ALL OCCURRENCES OF 'X' IN lv_conta WITH '0'.
    REPLACE ALL OCCURRENCES OF 'x' IN lv_conta WITH '0'.
    CONDENSE lv_conta NO-GAPS.
    lv_conta = |{ lv_conta ALPHA = IN }|.

    lv_conta_recebedor = i_reguh-zbnkn.
    REPLACE ALL OCCURRENCES OF '-' IN lv_conta_recebedor WITH ''.
    REPLACE ALL OCCURRENCES OF 'X' IN lv_conta_recebedor WITH '0'.
    REPLACE ALL OCCURRENCES OF 'x' IN lv_conta_recebedor WITH '0'.
    CONDENSE lv_conta_recebedor NO-GAPS.
    lv_conta_recebedor = |{ lv_conta_recebedor ALPHA = IN }|.

    ls_in_itau_pix-valor__pagamento = i_regup-dmbtr.
    REPLACE ALL OCCURRENCES OF '.' IN ls_in_itau_pix-valor__pagamento WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN ls_in_itau_pix-valor__pagamento WITH ''.
    CONDENSE ls_in_itau_pix-valor__pagamento NO-GAPS.
    lv_qtd = |{ ls_in_itau_pix-valor__pagamento ALPHA = IN }|.

*** US - 188109 - Inicio - CBRAND
    SELECT SINGLE bkref
       FROM lfbk
     INTO @lv_bkref
      WHERE lifnr EQ @i_regup-lifnr
        AND bvtyp EQ @i_regup-bvtyp. "BUG 193425 - CBRAND
    FIND lv_cp IN lv_bkref.
    IF sy-subrc = 0.
      DATA(lva_tp_conta_cp) = 'X'.
    ENDIF.

*** US - 188109 - Fim    - CBRAND


    ls_in_itau_pix =
    VALUE #(
              valor__pagamento = i_regup-dmbtr
              data__pagamento = |{ i_regup-zfbdt DATE = ISO }|
              ispb = lv_ispb
*** US - 188109 - Inicio - CBRAND
*tipo__identificacao__conta = i_reguh-hktid
              tipo__identificacao__conta = COND #( WHEN lva_tp_conta_cp IS NOT INITIAL THEN 'PP' ELSE i_reguh-hktid )
*** US - 188109 - Fim - CBRAND
              agencia__recebedor = i_reguh-zbnkl+lv_min(lv_max) "// PEGAR OS ULTIMOS 4 DIGITOS
              conta__recebedor = lv_conta_recebedor
              tipo__identificacao__recebedor = lv_f_j
              identificacao__recebedor = lv_cpf_cnpj
              informacoes__entre__usuarios = ls_bseg-sgtxt
              referencia__empresa = |{ i_regup-belnr }{ i_regup-buzei }|
              identificacao__comprovante = ls_bseg-sgtxt
              txid = |{ i_regup-bukrs }{ i_regup-belnr }{ i_regup-buzei }{ lv_qtd }|
              pagador =
                VALUE #(
                          tipo__conta = i_t012k-hktid
                          agencia = lv_bankl+lv_min1(lv_max1) "// PEGAR OS ULTIMOS 4 DIGITOS
                          conta = lv_conta
                          tipo__pessoa = 'J'
                          documento = i_cnpj
                          modulo__sispag = 'Fornecedores'
                         )
             ).

    CONDENSE ls_in_itau_pix-valor__pagamento NO-GAPS.

    REPLACE '/' IN ls_in_itau_pix-pagador-tipo__conta WITH ''.
    REPLACE '/' IN ls_in_itau_pix-tipo__identificacao__conta WITH ''.
    REPLACE '-' IN ls_in_itau_pix-conta__recebedor WITH ''.
    REPLACE '.' IN ls_in_itau_pix-txid WITH ''.
    REPLACE '-' IN ls_in_itau_pix-pagador-conta WITH ''.

    TRY.

        zcl_int_ob_fi_itau_pix=>zif_integracao_outbound~get_instance( )->execute_request(
          EXPORTING
            i_info_request         = ls_in_itau_pix
          IMPORTING
          e_id_integracao          = DATA(resul_id)
          e_integracao             = DATA(result_json)
        ).

      CATCH zcx_integracao INTO DATA(la_zcx_integracao).

*        MESSAGE ID LA_ZCX_INTEGRACAO->MSGID
*                TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
*                NUMBER LA_ZCX_INTEGRACAO->MSGNO
*                  WITH LA_ZCX_INTEGRACAO->MSGV1
*                       LA_ZCX_INTEGRACAO->MSGV2
*                       LA_ZCX_INTEGRACAO->MSGV3
*                       LA_ZCX_INTEGRACAO->MSGV4.
*        RETURN.

      CATCH zcx_error INTO DATA(zcx_error).

*        MESSAGE ID ZCX_ERROR->MSGID
*                TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
*                NUMBER ZCX_ERROR->MSGNO
*                  WITH ZCX_ERROR->MSGV1
*                       ZCX_ERROR->MSGV2
*                       ZCX_ERROR->MSGV3
*                       ZCX_ERROR->MSGV4.
*        RETURN.

    ENDTRY.


  ENDMETHOD.


  METHOD set_boletos_itau.

    DATA: lt_zfit0054     TYPE TABLE OF zfit0054,
          lt_zfit0056     TYPE TABLE OF zfit0056,
          lt_zfit0057     TYPE TABLE OF zfit0057,
          lv_guid         TYPE char40,
          lv_cod_arq      TYPE zfit0054-cod_arq,
          lv_seq_arq      TYPE zfit0056-seq_arq,
          lv_cod_mov      TYPE zfit0058-cod_mov,
          c_retorno_itau  TYPE zde_retorno_itau,
          lv_origem_receb TYPE zfit0054-origem_receb.

    IF is_pix IS INITIAL.
      CALL METHOD check_boleto_importado
        EXPORTING
          i_retorno_itau = i_retorno_itau
        CHANGING
          c_retorno_itau = c_retorno_itau.
    ELSE.
      c_retorno_itau = i_retorno_itau.
      "lv_origem_receb = 'PIX'. "US - 172238 - CBRAND
      " Comentei dia 24.09.2025 por causa de processo da amaggiON
    ENDIF.

    CHECK c_retorno_itau-data IS NOT INITIAL.

    lv_guid = i_retorno_itau-id_referencia.

    LOOP AT c_retorno_itau-data INTO DATA(ls_dados).

      lv_cod_arq = |COBR{ lc_banco-itau }{ sy-datum }{ sy-uzeit }{ sy-tabix }|.

*** US - 172238 - Inicio - CBRAND
      READ TABLE ls_dados-dado_boleto-dados_individuais_boleto INTO DATA(lwa_boleto) INDEX 1.
      IF lwa_boleto-situacao_geral_boleto = 'Baixada'.
        lv_origem_receb = 'PIX'.
      ELSE.
        IF lwa_boleto-situacao_geral_boleto = 'Paga'.
          lv_origem_receb = 'COD.BARRAS'.
        ENDIF.
      ENDIF.
*** US - 172238 - Fim - CBRAND

      APPEND VALUE #(
                      cod_arq = lv_cod_arq
                      banco   = lc_banco-itau
                      cnpj    = ls_dados-beneficiario-tipo_pessoa-cnpj_cpf
                      seq2    = ls_dados-id_boleto
                      api     = abap_true
                      guid    = lv_guid
                      origem_receb = lv_origem_receb "US - 172238 - CBRAND
                    ) TO lt_zfit0054.

      LOOP AT ls_dados-dado_boleto-dados_individuais_boleto INTO DATA(ls_boleto).

        CALL METHOD depara_status
          EXPORTING
            i_status = ls_boleto-situacao_geral_boleto
            i_baixa  = ls_dados-dado_boleto-baixa-motivo_baixa
            is_pix   = is_pix
          IMPORTING
            e_status = lv_cod_mov.

        REPLACE ALL OCCURRENCES OF '-' IN ls_boleto-data_vencimento WITH ''.
        REPLACE ALL OCCURRENCES OF '-' IN ls_dados-dado_boleto-data_emissao WITH ''.

        lv_seq_arq = sy-tabix.
        lv_seq_arq = |{ lv_seq_arq ALPHA = IN }|.

        ls_boleto-numero_nosso_numero = |{ ls_boleto-numero_nosso_numero ALPHA = OUT }|.

        APPEND VALUE #(
                        cod_arq       = lv_cod_arq
                        seq_arq       = lv_seq_arq
                        segmento      = 'T'
                        cod_retorno   = lv_cod_mov
                        nosso_nro     = ls_boleto-numero_nosso_numero
                        nro_doc_cobr  = ls_boleto-texto_seu_numero
                        dt_vcto       = ls_boleto-data_vencimento
                        vlr_titulo    = ls_boleto-valor_titulo
                        bco_receb     = lc_banco-itau
                        seq5          = ls_boleto-codigo_barras
                        cpudt         = sy-datum
                        cputm         = sy-uzeit
                        usnam         = sy-uname
                      ) TO lt_zfit0056.

        READ TABLE ls_dados-dado_boleto-pagamentos_cobranca TRANSPORTING NO FIELDS INDEX 1.
        IF sy-subrc IS NOT INITIAL.
          IF lv_cod_mov EQ '06'.

            APPEND VALUE #(
                    cod_arq          = lv_cod_arq
                    seq_arq          = lv_seq_arq
                    segmento         = 'U'
                    cod_retorno      = lv_cod_mov
                    vl_pago_sacado   = ls_boleto-valor_titulo
                    vl_liq_creditado = ls_boleto-valor_titulo
                    dt_ocorrencia    = ls_boleto-data_vencimento
                    dt_credito       = ls_dados-dado_boleto-data_emissao  "// US-172458 WBARBOSA 01/04/2025
                  ) TO lt_zfit0057.

          ENDIF.
        ENDIF.

      ENDLOOP.

      READ TABLE ls_dados-dado_boleto-pagamentos_cobranca INTO DATA(ls_cobranca) INDEX 1.
      IF sy-subrc IS INITIAL.

        REPLACE ALL OCCURRENCES OF '-' IN ls_cobranca-data_inclusao_pagamento WITH ''.

*** US - 178986 - CBRAND - Inicio
        IF lwa_boleto-situacao_geral_boleto = 'Baixada'.
          SPLIT ls_dados-dado_boleto-baixa-data_inclusao_alteracao_baixa AT 'T' INTO DATA(lv_dt_credito) DATA(lv_time).
          REPLACE ALL OCCURRENCES OF '-' IN lv_dt_credito WITH ''.
        ELSE.
          IF lwa_boleto-situacao_geral_boleto = 'Paga'.
            SPLIT ls_cobranca-data_inclusao_pagamento AT 'T' INTO lv_dt_credito lv_time.
            REPLACE ALL OCCURRENCES OF '-' IN lv_dt_credito WITH ''.
          ENDIF.
        ENDIF.
*** US - 178986 - CBRAND - Fim

        APPEND VALUE #(
                        cod_arq          = lv_cod_arq
                        seq_arq          = lv_seq_arq
                        segmento         = 'U'
                        cod_retorno      = lv_cod_mov
                        vl_pago_sacado   = ls_cobranca-valor_pago_total_cobranca
                        vl_liq_creditado = ls_cobranca-valor_pago_total_cobranca
                        dt_ocorrencia    = ls_cobranca-data_inclusao_pagamento
*** US - 178986 - CBRAND - Inicio
*                        dt_credito       = ls_dados-dado_boleto-data_emissao  "// US-172458 WBARBOSA 01/04/2025
                        dt_credito       = COND #( WHEN  lwa_boleto-situacao_geral_boleto = 'Baixada' OR
                           lwa_boleto-situacao_geral_boleto = 'Paga' THEN  lv_dt_credito  ELSE
                           ls_dados-dado_boleto-data_emissao
                          )
*** US - 178986 - CBRAND - Fim
                        vl_jr_mt_enc     = ls_cobranca-valor_pago_juro_cobranca + ls_cobranca-valor_pago_multa_cobranca
                        vl_desconto      = ls_cobranca-valor_pago_desconto_cobranca
                        vl_abatimento    = ls_cobranca-valor_pago_abatimento_cobranca
                      ) TO lt_zfit0057.
      ENDIF.
      CLEAR: lv_dt_credito, lv_origem_receb.

    ENDLOOP.

    SORT: lt_zfit0054,
          lt_zfit0056,
          lt_zfit0057.

    DELETE ADJACENT DUPLICATES FROM lt_zfit0054 COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM lt_zfit0056 COMPARING ALL FIELDS.
    DELETE ADJACENT DUPLICATES FROM lt_zfit0057 COMPARING ALL FIELDS.

    INSERT zfit0054 FROM TABLE lt_zfit0054.
    INSERT zfit0056 FROM TABLE lt_zfit0056.
    INSERT zfit0057 FROM TABLE lt_zfit0057.

    COMMIT WORK.

  ENDMETHOD.


  METHOD UPLOAD_AQUIVO_JSON_FROM_SAP.

    DATA: LV_PATH TYPE IBIPPARMS-PATH.
    DATA: LT_FILES TYPE TABLE OF STRING.
    DATA: E_RETORNO_ITAU  TYPE ZDE_RETORNO_ITAU.
    DATA: LV_JSON TYPE STRING.


    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        PROGRAM_NAME  = SYST-CPROG
        DYNPRO_NUMBER = SYST-DYNNR
      IMPORTING
        FILE_NAME     = LV_PATH.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        FILENAME = CONV STRING( LV_PATH )
      TABLES
        DATA_TAB = LT_FILES.

    LOOP AT LT_FILES INTO DATA(LS_FILES).
      LV_JSON = |{ LV_JSON }{ LS_FILES }|.
    ENDLOOP.

*"// Recupera os dados do JSON
    IF LV_JSON IS NOT INITIAL.

      LV_JSON = |{ LV_JSON CASE = UPPER }|.

      REPLACE 'NUMERO_CADASTRO_NACIONAL_PESSOA_JURIDICA'  IN LV_JSON WITH 'CNPJ_CPF'.
      REPLACE 'DESCRICAO_INSTRUMENTO_COBRANCA'            IN LV_JSON WITH 'DES_INS_COB'.
      REPLACE 'NUMERO_CADASTRO_PESSOA_FISICA'             IN LV_JSON WITH 'CNPJ_CPF'.
      REPLACE 'CODIGO_INSTITUICAO_FINANCEIRA_PAGAMENTO'   IN LV_JSON WITH 'COD_INS_FIN_PAG'.
      REPLACE 'COMANDAR_INSTRUCAO_ALTERAR_DADOS_COBRANCA' IN LV_JSON WITH 'COM_INS_ALT_DADOS_COB'.

      LV_JSON = |{ LV_JSON CASE = LOWER }|.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = LV_JSON
        CHANGING
          DATA = E_RETORNO_ITAU.

      E_RETORNO_ITAU-ID_REFERENCIA  = 'INPUT MANUAL'.

*      * "// Armazena os dados na Tabelas
      CALL METHOD SET_BOLETOS_ITAU
        EXPORTING
          I_RETORNO_ITAU = E_RETORNO_ITAU.

    ENDIF.


  ENDMETHOD.


  METHOD GET_COBRANCA_ITAU.

    TRY.

        ZCL_INT_OB_FI_ITAU_GET_COB=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE( )->EXECUTE_REQUEST(
          EXPORTING
            I_INFO_REQUEST         = I_REQUEST
          IMPORTING
          E_ID_INTEGRACAO          = DATA(RESUL_ID)
          E_INTEGRACAO             = DATA(RESULT_JSON)
        ).

      CATCH ZCX_INTEGRACAO INTO DATA(LA_ZCX_INTEGRACAO).

        MESSAGE ID LA_ZCX_INTEGRACAO->MSGID
                TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
                NUMBER LA_ZCX_INTEGRACAO->MSGNO
                  WITH LA_ZCX_INTEGRACAO->MSGV1
                       LA_ZCX_INTEGRACAO->MSGV2
                       LA_ZCX_INTEGRACAO->MSGV3
                       LA_ZCX_INTEGRACAO->MSGV4.
        RETURN.

      CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).

        MESSAGE ID ZCX_ERROR->MSGID
                TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
                NUMBER ZCX_ERROR->MSGNO
                  WITH ZCX_ERROR->MSGV1
                       ZCX_ERROR->MSGV2
                       ZCX_ERROR->MSGV3
                       ZCX_ERROR->MSGV4.
        RETURN.

    ENDTRY.

*"// Recupera os dados do JSON
    IF RESULT_JSON IS NOT INITIAL.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = RESULT_JSON-DS_DATA_RETORNO
        CHANGING
          DATA = E_RETORNO_ITAU.

      E_RETORNO_ITAU-ID_REFERENCIA  = RESUL_ID.

    ENDIF.

  ENDMETHOD.


  METHOD CHECK_ACESSO.

*    CALL METHOD GET_STVARV
*      EXPORTING
*        I_NAME = 'Z_USER_API_F110'
*        I_LOW  = CONV #( SY-UNAME )
*      RECEIVING
*        IS_OK  = DATA(IS_VALID).
*
*    CHECK IS_VALID IS NOT INITIAL.

    CALL METHOD GET_STVARV
      EXPORTING
        I_NAME = 'Z_CNPJ_API_F110'
        I_LOW  = CONV #( I_CNPJ )
      RECEIVING
        IS_OK  = DATA(IS_VALID).

    CHECK IS_VALID IS NOT INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD check_bolecode_itau.

    DATA ls_bolecode TYPE zde_in_itau_bolecode.

    TRY.
        DATA(lv_guid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(/afl/oref).
    ENDTRY.

    CALL METHOD zcl_fi_utils=>set_id_beneficiario
      EXPORTING
        i_empresa = i_empresa
        i_hbkid   = i_hbkid.

    CALL METHOD zcl_fi_utils=>get_id_beneficiario
      RECEIVING
        r_id_beneficiario = DATA(lv_id_beneficiario).

    IF i_nosso_numero IS NOT INITIAL.
* "// Popula os dados para chamada da API do Boleto
      DATA(ls_itau_boleto) =
        VALUE zde_in_itau_boleto(
           id_beneficiario = lv_id_beneficiario
           nosso_numero    = i_nosso_numero
           correlationid   = lv_guid
          ).

* "// Chama a API de Boleto
* "// Verifica se já existe um Boleto para Exibição
      CALL METHOD get_boletos_itau
        EXPORTING
          i_request      = ls_itau_boleto
        IMPORTING
          e_retorno_itau = e_retorno_itau.

      CALL METHOD get_bolecode_itau
        EXPORTING
          i_solicitacao = i_solicitacao
        RECEIVING
          r_zfit0227    = DATA(ls_zfit0227).

*// Atualiza os dados de Impressão do Boleto
      CALL METHOD update_inf_smart_boletos
        EXPORTING
          i_retorno_itau = e_retorno_itau
        CHANGING
          u_boleto       = c_dados_boleto.

      c_dados_boleto-qrcode = ls_zfit0227-emv.

      RETURN.
    ENDIF.

*"// Caso no Nosso Numero venha em Branco vamos criar o Boleto
*    CHECK I_NOSSO_NUMERO IS INITIAL.

*"// Recupera o proximo Nosso Numero
    CALL METHOD get_nosso_numero
      RECEIVING
        r_nosso_numero = c_dados_boleto-dtaid.

** BUG -189144 - Inicio - CBRAND
*** SALVA NA BSEG O NÚMERO: Motivo tempo de criar o boleto
    CALL METHOD document_change
      EXPORTING
        i_empresa     = i_empresa
        i_belnr       = i_belnr
        i_gjahr       = i_gjahr
        i_solicitacao = i_solicitacao
        i_dtaid       = c_dados_boleto-dtaid
        i_tipo        = i_tipo
        i_hbkid       = i_hbkid
      RECEIVING
        r_erro        = DATA(e_erro).

    IF e_erro = 'X'.
      RETURN.
    ENDIF.

** BUG -189144 - Fim - CBRAND

*"// Preenche Item do BoleCode
    CALL METHOD fill_bolecode
      EXPORTING
        i_smart_boleto = c_dados_boleto
      RECEIVING
        r_bolecode     = ls_bolecode.

    "// Chama a API de BoleCode
    CALL METHOD create_bolecode_itau
      EXPORTING
        i_request               = ls_bolecode
      IMPORTING
        e_retorno_bolecode_itau = DATA(e_retorno_bolecode_itau).

*"// Verifica se Retornou dados na Criação do Boleto
    CHECK e_retorno_bolecode_itau-data-dado_boleto-dados_individuais_boleto[] IS NOT INITIAL.

    READ TABLE e_retorno_bolecode_itau-data-dado_boleto-dados_individuais_boleto INTO DATA(ls_dados_individual_boleto) INDEX 1.

* "// Popula os dados para chamada da API do Boleto
    DATA(ls_itau_bolecode) =
      VALUE zde_in_itau_boleto(
         id_beneficiario = lv_id_beneficiario
         nosso_numero    = ls_dados_individual_boleto-numero_nosso_numero
         correlationid   = lv_guid
        ).

    WAIT UP TO 2 SECONDS.
* "// Chama a API de Boleto
* "// Verifica se já existe um Boleto para Exibição
    CALL METHOD get_boletos_itau
      EXPORTING
        i_request      = ls_itau_bolecode
      IMPORTING
        e_retorno_itau = e_retorno_itau.

*"// Armazena os dados no SAP na Tabela ZFIT0227
    CALL METHOD set_bolecode_itau
      EXPORTING
        i_solicitacao = i_solicitacao
        i_bolecode    = e_retorno_bolecode_itau.

*// Atualiza os dados de Impressão do Boleto
    CALL METHOD update_inf_smart_boletos
      EXPORTING
        i_retorno_itau = e_retorno_itau
      CHANGING
        u_boleto       = c_dados_boleto.

    CALL METHOD get_bolecode_itau
      EXPORTING
        i_solicitacao = i_solicitacao
      RECEIVING
        r_zfit0227    = ls_zfit0227.

    c_dados_boleto-qrcode = ls_zfit0227-emv.

* "// Armazena os dados na Tabelas
    CALL METHOD set_boletos_itau
      EXPORTING
        i_retorno_itau = e_retorno_itau.

  ENDMETHOD.


  METHOD CREATE_BOLECODE_ITAU.

    TRY.

        ZCL_INT_OB_FI_ITAU_BOLECODE=>ZIF_INTEGRACAO_OUTBOUND~GET_INSTANCE( )->EXECUTE_REQUEST(
            EXPORTING
              I_INFO_REQUEST         = I_REQUEST
            IMPORTING
            E_ID_INTEGRACAO          = DATA(RESUL_ID)
            E_INTEGRACAO             = DATA(RESULT_JSON)
          ).

      CATCH ZCX_ERROR INTO DATA(ZCX_ERROR).

        MESSAGE ID ZCX_ERROR->MSGID
                TYPE CL_ABAP_AAB_UTILITIES=>CATEGORY_ERROR
                NUMBER ZCX_ERROR->MSGNO
                  WITH ZCX_ERROR->MSGV1
                       ZCX_ERROR->MSGV2
                       ZCX_ERROR->MSGV3
                       ZCX_ERROR->MSGV4.
        RETURN.

    ENDTRY.

*"// Recupera os dados do JSON
    IF RESULT_JSON IS NOT INITIAL.

      REPLACE 'numero_cadastro_nacional_pessoa_juridica'  IN RESULT_JSON-DS_DATA_RETORNO WITH 'cnpj_cpf'.
      REPLACE 'descricao_instrumento_cobranca'            IN RESULT_JSON-DS_DATA_RETORNO WITH 'des_ins_cob'.
      REPLACE 'numero_cadastro_pessoa_fisica'             IN RESULT_JSON-DS_DATA_RETORNO WITH 'cnpj_cpf'.
      REPLACE 'codigo_instituicao_financeira_pagamento'   IN RESULT_JSON-DS_DATA_RETORNO WITH 'cod_ins_fin_pag'.
      REPLACE 'comandar_instrucao_alterar_dados_cobranca' IN RESULT_JSON-DS_DATA_RETORNO WITH 'com_ins_alt_dados_cob'.

*      RESULT_JSON-DS_DATA_RETORNO = |{ RESULT_JSON-DS_DATA_RETORNO CASE = UPPER }|.
*
*      REPLACE 'NUMERO_CADASTRO_NACIONAL_PESSOA_JURIDICA'  IN RESULT_JSON-DS_DATA_RETORNO WITH 'CNPJ_CPF'.
*      REPLACE 'DESCRICAO_INSTRUMENTO_COBRANCA'            IN RESULT_JSON-DS_DATA_RETORNO WITH 'DES_INS_COB'.
*      REPLACE 'NUMERO_CADASTRO_PESSOA_FISICA'             IN RESULT_JSON-DS_DATA_RETORNO WITH 'CNPJ_CPF'.
*      REPLACE 'CODIGO_INSTITUICAO_FINANCEIRA_PAGAMENTO'   IN RESULT_JSON-DS_DATA_RETORNO WITH 'COD_INS_FIN_PAG'.
*      REPLACE 'COMANDAR_INSTRUCAO_ALTERAR_DADOS_COBRANCA' IN RESULT_JSON-DS_DATA_RETORNO WITH 'COM_INS_ALT_DADOS_COB'.
*
*      RESULT_JSON-DS_DATA_RETORNO = |{ RESULT_JSON-DS_DATA_RETORNO CASE = LOWER }|.

      CALL METHOD /UI2/CL_JSON=>DESERIALIZE
        EXPORTING
          JSON = RESULT_JSON-DS_DATA_RETORNO
        CHANGING
          DATA = E_RETORNO_BOLECODE_ITAU.

    ENDIF.

  ENDMETHOD.


  METHOD FILL_BOLECODE.

    DATA: LV_ID_BENEFICIARIO TYPE ZDE_ID_BENEFICIARIO.

    DATA(XLEN) = STRLEN( I_SMART_BOLETO-XESTCD1 ).
    DATA(LEN) = STRLEN( I_SMART_BOLETO-STCD1 ).

    CALL METHOD ZCL_FI_UTILS=>GET_ID_BENEFICIARIO
      RECEIVING
        R_ID_BENEFICIARIO = LV_ID_BENEFICIARIO.

    R_BOLECODE =
   VALUE #(
             ETAPA_PROCESSO_BOLETO = 'efetivacao'
*             ETAPA_PROCESSO_BOLETO = 'simulacao'

* "// Dados do Beneficiario
             BENEFICIARIO-ID_BENEFICIARIO  = LV_ID_BENEFICIARIO
             BENEFICIARIO-NOME_COBRANCA    = I_SMART_BOLETO-XENAME1

* "// Dados do Beneficiario - Caracteristica
             BENEFICIARIO-TIPO_PESSOA-CODIGO_TIPO_PESSOA = COND #( WHEN XLEN EQ 11 THEN 'F' ELSE 'J')
             BENEFICIARIO-TIPO_PESSOA-CNPJ_CPF           = I_SMART_BOLETO-XESTCD1

* "// Dados do Beneficiario - Endereço
             BENEFICIARIO-ENDERECO-NOME_LOGRADOURO = I_SMART_BOLETO-XESTRAS
             BENEFICIARIO-ENDERECO-NOME_BAIRRO     = I_SMART_BOLETO-XEORT02
             BENEFICIARIO-ENDERECO-NOME_CIDADE     = I_SMART_BOLETO-XEORT01
             BENEFICIARIO-ENDERECO-SIGLA_UF        = I_SMART_BOLETO-XEREGIO
             BENEFICIARIO-ENDERECO-NUMERO_CEP      = I_SMART_BOLETO-XEPSTLZ

* "// Dados do Boleto
             DADO_BOLETO-DES_INS_COB             = 'boleto_pix'
             DADO_BOLETO-TIPO_BOLETO             = 'a vista'
             DADO_BOLETO-FORMA_ENVIO             = 'impressão'
             DADO_BOLETO-QUANTIDADE_PARCELAS     = '1'
             DADO_BOLETO-CODIGO_CARTEIRA         = '109'
             DADO_BOLETO-CODIGO_TIPO_VENCIMENTO  = '3'
             DADO_BOLETO-CODIGO_ESPECIE          = '01'

             DADO_BOLETO-PAGADOR-PESSOA-NOME_PESSOA = I_SMART_BOLETO-NAME1

* "// Dados do Pagador - Caracteristica
             DADO_BOLETO-PAGADOR-PESSOA-TIPO_PESSOA-CODIGO_TIPO_PESSOA = COND #( WHEN LEN EQ 11 THEN 'F' ELSE 'J')
             DADO_BOLETO-PAGADOR-PESSOA-TIPO_PESSOA-CNPJ_CPF           = I_SMART_BOLETO-STCD1

* "// Dados do Pagador - Endereço
             DADO_BOLETO-PAGADOR-ENDERECO-NOME_LOGRADOURO  = I_SMART_BOLETO-STRAS
             DADO_BOLETO-PAGADOR-ENDERECO-NOME_BAIRRO      = I_SMART_BOLETO-ORT02
             DADO_BOLETO-PAGADOR-ENDERECO-NOME_CIDADE      = I_SMART_BOLETO-ORT01
             DADO_BOLETO-PAGADOR-ENDERECO-SIGLA_UF         = I_SMART_BOLETO-REGIO
             DADO_BOLETO-PAGADOR-ENDERECO-NUMERO_CEP       = I_SMART_BOLETO-PSTLZ

* "// Dados individual do Boleto
             DADO_BOLETO-DADOS_INDIVIDUAIS_BOLETO = VALUE #( (
                     NUMERO_NOSSO_NUMERO   = I_SMART_BOLETO-DTAID
                     VALOR_TITULO          = I_SMART_BOLETO-DMBTR
                     DATA_VENCIMENTO       = I_SMART_BOLETO-ZBD1T
              ) )
   ).

  ENDMETHOD.


  METHOD GET_BOLECODE_ITAU.

    SELECT SINGLE *
      FROM ZFIT0227
      INTO R_ZFIT0227
      WHERE SIMULACAO EQ I_SOLICITACAO
      AND DELETADO EQ ABAP_FALSE.

  ENDMETHOD.


  METHOD GET_ID_BENEFICIARIO.
    R_ID_BENEFICIARIO = ZCL_FI_UTILS=>AT_ID_BENEFICIARIO.
  ENDMETHOD.


  METHOD GET_NOSSO_NUMERO.

    DATA: LV_NR TYPE CHAR02.

    LV_NR = |{ SY-DATUM+2(2) }|.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR = LV_NR
        OBJECT      = 'ZSEQBOLETO'
      IMPORTING
        NUMBER      = R_NOSSO_NUMERO.

  ENDMETHOD.


  METHOD GET_STVARV.

    SELECT COUNT(*)
      FROM TVARVC
      WHERE NAME EQ @I_NAME
        AND LOW  EQ @I_LOW.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_TRUE.

  ENDMETHOD.


  METHOD SET_BOLECODE_ITAU.

    READ TABLE I_BOLECODE-DATA-DADO_BOLETO-DADOS_INDIVIDUAIS_BOLETO INTO DATA(LS_BOLECODE) INDEX 1.

    DATA(LS_ZFIT0227) =
    VALUE ZFIT0227(
              SIMULACAO              = I_SOLICITACAO
              NOSSO_NUMERO           = LS_BOLECODE-NUMERO_NOSSO_NUMERO
              EMV                    = I_BOLECODE-DATA-DADOS_QRCODE-EMV
              TXID                   = I_BOLECODE-DATA-DADOS_QRCODE-TXID
              CODIGO_BARRAS          = LS_BOLECODE-CODIGO_BARRAS
              NUMERO_LINHA_DIGITAVEL = LS_BOLECODE-NUMERO_LINHA_DIGITAVEL
              DATA                   = SY-DATUM
              HORA                   = SY-UZEIT
           ).

    UPDATE ZFIT0227
          SET DELETADO = ABAP_TRUE
      WHERE SIMULACAO EQ I_SOLICITACAO.
    IF SY-SUBRC IS INITIAL.
      COMMIT WORK.
    ENDIF.

    INSERT ZFIT0227 FROM LS_ZFIT0227.
    IF SY-SUBRC IS INITIAL.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD SET_ID_BENEFICIARIO.

    SELECT SINGLE BANKL
      FROM T012
      INTO @DATA(LV_BANKL)
      WHERE BUKRS EQ @I_EMPRESA
        AND HBKID EQ @I_HBKID.

    IF SY-SUBRC IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE BANKN
      FROM T012K
      INTO @DATA(LV_BANKN)
      WHERE BUKRS EQ @I_EMPRESA
        AND HBKID EQ @I_HBKID.

    IF SY-SUBRC IS NOT INITIAL.
      RETURN.
    ENDIF.

    REPLACE '-' IN LV_BANKN WITH ''.
    LV_BANKN = |{ LV_BANKN ALPHA = IN }|.
    DATA(LEN1) = STRLEN( LV_BANKN ).
    DATA(INI) = LEN1 - 8.

    ZCL_FI_UTILS=>AT_ID_BENEFICIARIO = |{ LV_BANKL+4(4) }{ LV_BANKN+INI }|.

  ENDMETHOD.


  METHOD update_inf_smart_boletos.

    IF i_retorno_itau-data IS INITIAL.
      CLEAR u_boleto.
      RETURN.
    ENDIF.

    READ TABLE i_retorno_itau-data INTO DATA(ls_boleto) INDEX 1.
    CHECK sy-subrc IS INITIAL.

    u_boleto-data_sist = sy-datum.

    u_boleto-xename1 = |{ ls_boleto-beneficiario-nome_cobranca CASE = UPPER }|.
    u_boleto-xestcd1 = ls_boleto-beneficiario-tipo_pessoa-cnpj_cpf.
    u_boleto-xestras = |{ ls_boleto-beneficiario-endereco-nome_logradouro CASE = UPPER }|.
    u_boleto-xeort02 = |{ ls_boleto-beneficiario-endereco-nome_bairro CASE = UPPER }|.
    u_boleto-xeort01 = |{ ls_boleto-beneficiario-endereco-nome_cidade CASE = UPPER }|.
    u_boleto-xeregio = |{ ls_boleto-beneficiario-endereco-sigla_uf CASE = UPPER }|.
    u_boleto-xepstlz = ls_boleto-beneficiario-endereco-numero_cep.

    u_boleto-name1 = |{ ls_boleto-dado_boleto-pagador-pessoa-nome_pessoa CASE = UPPER }|.
    u_boleto-stcd1 = ls_boleto-dado_boleto-pagador-pessoa-tipo_pessoa-cnpj_cpf.
    u_boleto-stras = |{ ls_boleto-dado_boleto-pagador-endereco-nome_logradouro CASE = UPPER }|.
    u_boleto-ort01 = |{ ls_boleto-dado_boleto-pagador-endereco-nome_cidade CASE = UPPER }|.
    u_boleto-regio = |{ ls_boleto-dado_boleto-pagador-endereco-sigla_uf CASE = UPPER }|.
    u_boleto-pstlz = ls_boleto-dado_boleto-pagador-endereco-numero_cep.

    u_boleto-qrcode = ls_boleto-dado_boleto-qrcode_pix-emv.

    READ TABLE ls_boleto-dado_boleto-dados_individuais_boleto INTO DATA(ls_dados_individual) INDEX 1.


    u_boleto-zbd1t = replace( val   = ls_dados_individual-data_vencimento
                              regex = '-'
                              with  = ''
                              occ   = 0
                              ).

    u_boleto-dmbtr = ls_dados_individual-valor_titulo.

    u_boleto-var_cod_barras_fim = ls_dados_individual-codigo_barras.

    u_boleto-dtaid  = |109{ ls_dados_individual-numero_nosso_numero }|.
*    U_BOLETO-DTAID  = |190{ LS_DADOS_INDIVIDUAL-NUMERO_NOSSO_NUMERO }|.

    WRITE ls_dados_individual-numero_linha_digitavel
      USING EDIT MASK '_____._____._____.______ _____.______ _ ______________'
    TO u_boleto-var_linha_dig.

  ENDMETHOD.


  METHOD document_change.
    DATA: t_bseg   TYPE TABLE OF bseg,
          t_accchg TYPE TABLE OF accchg,
          v_buzei	 TYPE	 bseg-buzei,
          v_bukrs  TYPE  accit-bukrs,
          v_belnr  TYPE  bseg-belnr,
          v_gjahr  TYPE  bseg-gjahr.

    v_bukrs = i_empresa.
    v_belnr = i_belnr.
    v_gjahr = i_gjahr.

    CALL FUNCTION 'FI_DOCUMENT_READ'
      EXPORTING
        i_bukrs     = v_bukrs
        i_belnr     = v_belnr
        i_gjahr     = v_gjahr
      TABLES
        t_bseg      = t_bseg
      EXCEPTIONS
        wrong_input = 1
        not_found   = 2
        OTHERS      = 3.

    READ TABLE t_bseg INTO DATA(wl_bseg) WITH KEY bschl = '01'.
    IF sy-subrc NE 0.
      READ TABLE t_bseg INTO wl_bseg WITH KEY bschl = '09'.
    ENDIF.
    IF sy-subrc EQ 0.
      APPEND INITIAL LINE TO t_accchg ASSIGNING FIELD-SYMBOL(<fs_accchg>).
      <fs_accchg>-fdname = 'XREF2'.
      <fs_accchg>-newval = i_solicitacao.

      APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
      <fs_accchg>-fdname = 'XREF3'.
      <fs_accchg>-newval = i_dtaid.

*    APPEND INITIAL LINE TO t_accchg ASSIGNING <fs_accchg>.
*    <fs_accchg>-fdname = 'HBKID'.
*    <fs_accchg>-newval = p_hbkid.

      v_buzei = wl_bseg-buzei.

      CALL FUNCTION 'FI_DOCUMENT_CHANGE'
        EXPORTING
          i_buzei              = v_buzei
          i_bukrs              = v_bukrs
          i_belnr              = v_belnr
          i_gjahr              = v_gjahr
        TABLES
          t_accchg             = t_accchg
        EXCEPTIONS
          no_reference         = 1
          no_document          = 2
          many_documents       = 3
          wrong_input          = 4
          overwrite_creditcard = 5
          OTHERS               = 6.

      IF sy-subrc NE 0.
        r_erro = 'X'.
      ENDIF.
      CLEAR: t_accchg[],t_bseg[],v_bukrs,v_belnr,v_gjahr,v_buzei.
    ENDIF.
  ENDMETHOD.


  METHOD send_pix_bradesco.
    DATA: ls_in_bbd_pix      TYPE zde_in_bbd_pix,
          lv_cpf_cnpj        TYPE string,
          lv_conta           TYPE c LENGTH 8,
          lv_conta_recebedor TYPE c LENGTH 18,
          lv_dig_recebedor   TYPE c LENGTH 02,
          lv_qtd             TYPE c LENGTH 17,
          lv_bkref           TYPE lfbk-bkref,
          lv_cp              TYPE string,
          lt_retorno         TYPE zde_in_bbd_pix_ret,
          lt_zfit0237        TYPE TABLE OF zfit0237,
          lw_zfit0237        LIKE LINE  OF lt_zfit0237.

    lv_cp = 'POUPAN'.

    SELECT SINGLE bankl
      FROM t012
      INTO @DATA(lv_bankl)
      WHERE hbkid EQ @i_t012k-hbkid
        AND bukrs EQ @i_t012k-bukrs.

    SELECT SINGLE lifnr, stkzn, stcd1, stcd2
      FROM lfa1
      INTO @DATA(ls_lfa1)
      WHERE lifnr EQ @i_regup-lifnr.


*** BUG - 192416  - 06.10.2025 - CBRAND - Inicio
    SELECT SINGLE businesspartner
      FROM ibupasuplrcotp
      INTO @DATA(lv_partner)
      WHERE supplier EQ @i_regup-lifnr.

*    SELECT SINGLE bankl
*      FROM but0bk
*      INTO @DATA(lv_codigo_banco)
*      WHERE partner EQ @i_regup-lifnr
*      AND bkvid EQ @i_regup-bvtyp.

    SELECT SINGLE bankl
      FROM but0bk
      INTO @DATA(lv_codigo_banco)
      WHERE partner EQ @lv_partner
      AND bkvid EQ @i_regup-bvtyp.
*** BUG - 192416  - 06.10.2025 - CBRAND - Fim

    SELECT SINGLE ispb_code
      FROM fiapbrd_ispb
      INTO @DATA(lv_ispb)
      WHERE bukrs EQ @i_regup-bukrs
      AND bank_key EQ @lv_codigo_banco(3).


    DATA(lv_f_j) = COND #( WHEN ls_lfa1-stkzn IS NOT INITIAL
                       THEN 'F'
                       ELSE 'J'
                    ).

    lv_cpf_cnpj = SWITCH #( lv_f_j
                              WHEN 'F' THEN ls_lfa1-stcd2
                              WHEN 'J' THEN ls_lfa1-stcd1
                          ).


    SELECT SINGLE belnr, gjahr, buzei, sgtxt
      FROM bseg
      INTO @DATA(ls_bseg)
      WHERE belnr EQ @i_regup-belnr
        AND gjahr EQ @i_regup-gjahr
        AND buzei EQ @i_regup-buzei.

    DATA(lv_max) = strlen( i_reguh-zbnkl  ).
    IF lv_max >= 4.
      DATA(lv_min) = lv_max - 4 .
    ENDIF.

    DATA(lv_max1) = strlen( lv_bankl ).
    IF lv_max1 >= 4.
      DATA(lv_min1) = lv_max1 - 4 .
    ENDIF.

    lv_conta = i_t012k-bankn.
    REPLACE ALL OCCURRENCES OF '-' IN lv_conta WITH ''.
    REPLACE ALL OCCURRENCES OF 'X' IN lv_conta WITH '0'.
    REPLACE ALL OCCURRENCES OF 'x' IN lv_conta WITH '0'.
    CONDENSE lv_conta NO-GAPS.
    lv_conta = |{ lv_conta ALPHA = IN }|.

    " lv_conta_recebedor = i_reguh-zbnkn. "COM DIGITO

    SPLIT i_reguh-zbnkn AT '-'
      INTO lv_conta_recebedor             "Número da C/C
           lv_dig_recebedor.              "Digito da Conta Corrente

    REPLACE ALL OCCURRENCES OF '-' IN lv_conta_recebedor WITH ''.
    REPLACE ALL OCCURRENCES OF 'X' IN lv_conta_recebedor WITH '0'.
    REPLACE ALL OCCURRENCES OF 'x' IN lv_conta_recebedor WITH '0'.
    CONDENSE lv_conta_recebedor NO-GAPS.
    lv_conta_recebedor = |{ lv_conta_recebedor ALPHA = IN }|.

    ls_in_bbd_pix-valor = i_regup-dmbtr.
    REPLACE ALL OCCURRENCES OF '.' IN ls_in_bbd_pix-valor WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN ls_in_bbd_pix-valor WITH ''.
    CONDENSE ls_in_bbd_pix-valor NO-GAPS.
    lv_qtd = |{ ls_in_bbd_pix-valor ALPHA = IN }|.


    ls_in_bbd_pix =
      VALUE #(
      idtransacao = |{ i_regup-bukrs }{ i_regup-belnr }{ i_regup-buzei }{ lv_qtd }|
      pagador =
                  VALUE #(
                           tipochave =  'AGENCIACONTA'
                           agencia   = lv_bankl+lv_min1(lv_max1)
                           conta     = lv_conta+1(6)
                           )
      recebedor =
                  VALUE #(
                           cpfCnpj        = lv_cpf_cnpj
                           tipoChave      = 'AGENCIACONTA'
                           agencia        = i_reguh-zbnkl+lv_min(lv_max)
                           banco          = lv_codigo_banco(3)
                           ispb           = lv_ispb
                           conta          = lv_conta_recebedor
                           digitoConta    = lv_dig_recebedor
                           "tipoConta
                           nomeFavorecido = i_reguh-name1
                          )
       valor      = i_regup-dmbtr
       descricao  = |{ i_regup-belnr }{ i_regup-buzei }|
       ).

    CONDENSE ls_in_bbd_pix-valor NO-GAPS.

    REPLACE '-' IN ls_in_bbd_pix-recebedor-conta WITH ''.
    REPLACE '.' IN ls_in_bbd_pix-idtransacao     WITH ''.
    REPLACE '-' IN ls_in_bbd_pix-pagador-conta   WITH ''.

    TRY.
        zcl_int_ob_fi_bradesco_pix=>zif_integracao_outbound~get_instance( )->execute_request(
          EXPORTING
            i_info_request         = ls_in_bbd_pix
          IMPORTING
          e_id_integracao          = DATA(resul_id)
          e_integracao             = DATA(result_json)
        ).
        IF result_json-ds_data_retorno IS NOT INITIAL AND result_json-nm_code = '0200'.
          /ui2/cl_json=>deserialize( EXPORTING json = result_json-ds_data_retorno CHANGING data = lt_retorno ).

          CLEAR: lw_zfit0237,
                 lt_zfit0237.

          lw_zfit0237-mandt                        = sy-mandt.
          lw_zfit0237-bukrs                        = i_t012k-bukrs.
          lw_zfit0237-hbkid                        = i_t012k-hbkid.
          lw_zfit0237-idtransacao                  = lt_retorno-idtransacao.
          lw_zfit0237-cpfcnpj_pgdr                 = i_cnpj.
          lw_zfit0237-agencia_pgdr                 = lt_retorno-pagador-agencia.
          lw_zfit0237-conta_pgdr                   = lt_retorno-pagador-conta.
          lw_zfit0237-tipoconta_pgdr               = ''.
          lw_zfit0237-cpfcnpj_receb                = lt_retorno-recebedor-cpfcnpj.
          lw_zfit0237-tipochave_receb              = lt_retorno-recebedor-tipochave.
          lw_zfit0237-tipoconta_receb              = lt_retorno-recebedor-tipoconta.
          lw_zfit0237-ispb_receb                   = lt_retorno-recebedor-ispb.
          lw_zfit0237-agencia_receb                = lt_retorno-recebedor-agencia.
          lw_zfit0237-conta_receb                  = lt_retorno-recebedor-conta.
          lw_zfit0237-banco_receb                  = lt_retorno-recebedor-banco.
          lw_zfit0237-nomefavorecido_receb         = lt_retorno-recebedor-nomefavorecido.
          lw_zfit0237-valor                        = lt_retorno-valor.
          lw_zfit0237-e2e                          = lt_retorno-e2e.
          lw_zfit0237-descricao                    = lt_retorno-descricao.

          DATA(lv_data_cria) = lt_retorno-datacriacao+0(10).
          REPLACE ALL OCCURRENCES OF '-' IN lv_data_cria  WITH ''.
          CONDENSE lv_data_cria NO-GAPS.

          lw_zfit0237-datacriacao                  = lv_data_cria.
          lw_zfit0237-status                       = lt_retorno-status.
          lw_zfit0237-valortarifa                  = lt_retorno-valortarifa.
          lw_zfit0237-mensagem                     = lt_retorno-motivo.
          "lw_zfit0237-cod_retorno                  = result_json-nm_code.
          lw_zfit0237-dt_registro                  = sy-datum.
          lw_zfit0237-hr_registro                  = sy-uzeit.
          lw_zfit0237-us_registro                  = sy-uname.

          APPEND lw_zfit0237 TO lt_zfit0237.

          MODIFY zfit0237 FROM TABLE lt_zfit0237.
          COMMIT WORK AND WAIT.

        ENDIF.

      CATCH zcx_integracao INTO DATA(la_zcx_integracao).
      CATCH zcx_error INTO DATA(zcx_error).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
