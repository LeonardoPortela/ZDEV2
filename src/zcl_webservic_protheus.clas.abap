class ZCL_WEBSERVIC_PROTHEUS definition
  public
  final
  create public .

public section.

  class-methods GET_DADOS_FATURA
    importing
      !I_FATURA type RE_BELNR
      !I_CNPJ type STCD1 .
  class-methods SERVER
    importing
      !VIEW type RS38L_FNAM
    changing
      !REQUEST type ref to IF_HTTP_REQUEST
      !RESPONSE type ref to IF_HTTP_RESPONSE
    returning
      value(DATA) type STRING .
  class-methods GET_CUPOM
    returning
      value(E_CUPOM) type ZPMD014 .
  class-methods CONS_FATURA
    importing
      !I_FATURA type RE_BELNR
      !I_CUPOM type ZPMD017 .
  class-methods GET_FATURA
    returning
      value(E_FATURA) type ZPMD015 .
  class-methods SEL_FORNECEDOR
    importing
      !I_CNPJ type J_1BCGC
      !I_IE type J_1BSTAINS
      !I_EMPRESA type BUKRS
    exporting
      !E_LIFNR type LIFNR .
  class-methods GET_CONS_FATURA
    importing
      !FATURA type CHAR10
      !CNPJ type STCD1
    returning
      value(E_RETURNG) type ZPME0037 .
  class-methods GET_NFE
    returning
      value(E_NFE) type ZPMD015 .
  class-methods POST_FATURA
    importing
      !I_FATURA type ZPME0032_T
    returning
      value(E_FATURA) type ZPME0033_T .
  class-methods SET_PEDIDO
    importing
      !T_FATURA type ZPME0042_T
      !T_OBS type ZPME0045_T
    exporting
      value(E_MESSAGE) type BAPI_MSG
      value(E_PEDIDO) type EBELN
    returning
      value(E_RETURNG) type ZPME0041_T .
  class-methods GET_C_C_EQUIP
    importing
      !AUFNR type AUFNR
    exporting
      !KOSTL type KOSTL .
  class-methods GET_CONTA_RAZAO
    importing
      !MATNR type MATNR
    exporting
      !SAKNR type SAKNR .
*---> 20.06.2023 - Migração S4 - DG
"      !I_STATUS type CHAR02 .
  class-methods GET_STATUS_PEDIDO
    importing
      !I_FATURA type RE_BELNR
      !CNPJ type STCD1
    exporting
      !I_STATUS type ZCHAR02 .
*<--- 20.06.2023 - Migração S4 - DG
  class-methods GET_ORDEM
    importing
      !WERKS type WERKS_D
    exporting
      !AUFNR type AUFNR .
  class-methods GET_IMPOSTO_ITEM_PEDIDO
    importing
      !I_WERKS type WERKS_D
      !I_EBELP type EBELP
      !I_EBELN type EBELN
      !I_MENGE type MENGE
      !I_NETPR type NETPR
      !I_MWSKZ type MWSKZ
      !I_PEINH type PEINH
    exporting
      !E_VALOR type NETWR
      !E_WMWST type WMWST .
*---> 20.06.2023 - Migração S4 - DG
"      !I_STATUS type CHAR02
*<--- 20.06.2023 - Migração S4 - DG
  class-methods SET_SATUS_PEDIDO
    importing
      !I_STATUS type ZCHAR02
      !I_FATURA type RE_BELNR
      !I_CNPJ type STCD1 .
  class-methods CALL_REPORT
    importing
      !I_SEQUEN type STRING
      !I_REPORT type PROGNAME
      !I_UNAME type UNAME optional
      !I_EXTERNAL type FLAG optional .
  class-methods SET_RATEIO_ZIB_CONTABIL .
  class-methods SET_DATA_VENC_PEDIDO
    importing
      !I_PEDIDO type EBELN
      !I_DT_EMISSAO_NFE type DATUM
    exporting
      !E_DATA_VENC type DATUM .
  class-methods SET_CANC_PEDIDO
    importing
      value(I_EBELN) type EBELN optional .
*---> 20.06.2023 - Migração S4 - DG
"      !E_STATUS type CHAR02 .
*<--- 20.06.2023 - Migração S4 - DG
  class-methods GET_EMPRESA_CFOP
    importing
      !I_NOTA type ZIB_NFE_DIST_TER
    exporting
      !E_STATUS type ZCHAR02 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WEBSERVIC_PROTHEUS IMPLEMENTATION.


  METHOD call_report.

    DATA:
      v_jobd     LIKE sy-datum,
      v_jobt     LIKE sy-uzeit,
      v_back(1)  TYPE c,
      v_aux_1(1) TYPE c,
      v_aux_2(2) TYPE c.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname,
          print_parameters TYPE pri_params.

    name = |JOB_{ i_report }_{ i_sequen }|.

    IF i_external IS INITIAL. " 31.10.2024 - RAMON - 156855

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = name
        IMPORTING
          jobcount         = number
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      IF sy-subrc IS INITIAL.

        IF i_uname IS INITIAL.
          SUBMIT (i_report) VIA JOB name
                            NUMBER number
                            AND RETURN.
        ELSE.
          SUBMIT (i_report) VIA JOB name
                            NUMBER number
                              USER i_uname
                            AND RETURN.
        ENDIF.

        IF sy-subrc IS INITIAL.
          DATA(immediate) = 'X'.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobname              = name
              jobcount             = number
              strtimmed            = immediate
            EXCEPTIONS
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              OTHERS               = 8.

        ENDIF.
      ENDIF.

    ELSE.  " 31.10.2024 - RAMON - 156855

      "solicitar e Aguardar execução do job
      zcl_job=>insert_job_fila_escalonamento( EXPORTING
                                                i_nome_job = name
                                                i_report = i_report
                                                i_user_job = i_uname
                                              IMPORTING
                                                e_jobcount =   DATA(e_jobcount)
                                                e_jobname  =   DATA(e_jobname) ).
    ENDIF.


  ENDMETHOD.


  METHOD CONS_FATURA.
    DATA: W_FATURA TYPE ZPMT0024.

    DATA(FATURA) = |{ I_FATURA ALPHA = IN }|.
    DATA(CUPOM)  = |{ I_CUPOM ALPHA = IN }|.


    SELECT *
    FROM ZPMT0024
    INTO TABLE @DATA(_FATURA)
      WHERE FATURA EQ @FATURA
       AND  CUPOM_FISC EQ @CUPOM .

      LOOP AT _FATURA ASSIGNING FIELD-SYMBOL(<WS_FATURA>).

      MOVE-CORRESPONDING <WS_FATURA> TO W_FATURA.
      W_FATURA-STATUS_PROC = 'erro'.

      MODIFY ZPMT0024 FROM W_FATURA.
      COMMIT WORK.
      CLEAR W_FATURA.
    ENDLOOP.
  ENDMETHOD.


  METHOD GET_CONS_FATURA.

    DATA: T_ZPMT0024 TYPE TABLE OF ZPMT0024.
    DATA: T_ZPMT0032 TYPE TABLE OF ZPMT0032.
    DATA: GT_ZPMT0024 TYPE TABLE OF ZPMT0024.
    DATA: T_ZPME0037 TYPE ZPME0037_T.
    FREE: T_ZPMT0024, GT_ZPMT0024, T_ZPME0037.
    DATA: _FATURA TYPE CHAR10.

*Consulta faturas .
    _FATURA = |{ FATURA ALPHA = IN }|.

**Consultando status do pedido.
    ZCL_WEBSERVIC_PROTHEUS=>GET_STATUS_PEDIDO(
            EXPORTING
      I_FATURA =  _FATURA
          CNPJ = CNPJ
    IMPORTING
      I_STATUS = DATA(W_STATUS)
    ).


    SELECT *
    FROM ZPMT0032
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMT0032
     WHERE FATURA EQ _FATURA
      AND CNPJ EQ CNPJ.

    SELECT *
    FROM ZPMT0024
    INTO CORRESPONDING FIELDS OF TABLE T_ZPMT0024
      FOR ALL ENTRIES IN T_ZPMT0032
     WHERE FATURA EQ T_ZPMT0032-FATURA
         AND CNPJ EQ T_ZPMT0032-CNPJ.

    SORT T_ZPMT0024 ASCENDING BY FATURA.
    DELETE T_ZPMT0024 WHERE COD_STATUS NE '999'.


    IF T_ZPMT0032 IS NOT INITIAL.
*      GT_ZPMT0024 = T_ZPMT0024.
*      SORT GT_ZPMT0024 ASCENDING BY FATURA.
*      DELETE ADJACENT DUPLICATES FROM GT_ZPMT0024 COMPARING FATURA.

*******Selecionado dados da fatura.
      T_ZPME0037 = VALUE #( FOR LW IN T_ZPMT0032 ( FATURA  = LW-FATURA
                                               COD_RETORN  = SWITCH #( LW-COD_STATUS WHEN SPACE THEN 0 ELSE |{ LW-COD_STATUS ALPHA = OUT }| )
                                                    PEDIDO = LW-PEDIDO
                                              ) ).

      LOOP AT T_ZPME0037 ASSIGNING FIELD-SYMBOL(<_ZPME0037>).
        IF <_ZPME0037>-COD_RETORN EQ '999'.
          LOOP AT T_ZPMT0024 ASSIGNING FIELD-SYMBOL(<WA_FATURA>) WHERE FATURA EQ <_ZPME0037>-FATURA.
            APPEND VALUE #( CUPOM_FISC     = <WA_FATURA>-CUPOM_FISC
                            SERIE_CUPOM    = <WA_FATURA>-SERIE_CUPOM
                            OBSERV         = <WA_FATURA>-OBSERV )  TO <_ZPME0037>-DADOS.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    FREE: E_RETURNG.
    LOOP AT T_ZPME0037 ASSIGNING FIELD-SYMBOL(<_RETURN>).
      E_RETURNG = VALUE #( COD_RETORN = <_RETURN>-COD_RETORN
                               FATURA = <_RETURN>-FATURA
                               PEDIDO = <_RETURN>-PEDIDO
                              DADOS = VALUE #( FOR LS IN <_RETURN>-DADOS
                                             ( CUPOM_FISC = LS-CUPOM_FISC
                                              SERIE_CUPOM = LS-SERIE_CUPOM
                                              OBSERV      = LS-OBSERV
                                                         ) ) ).


*      E_RETURNG = T_ZPME0037.
    ENDLOOP.




  ENDMETHOD.


  METHOD GET_CONTA_RAZAO.

    DATA(W_MATNR) = |{ MATNR ALPHA = IN }|.

    SELECT SINGLE MATKL
    FROM MARA
    INTO @DATA(_MATKL)
      WHERE MATNR EQ @W_MATNR.

    CHECK _MATKL IS NOT INITIAL.

    SELECT SINGLE SAKNR
    FROM ZMMT0039
    INTO SAKNR
      WHERE MATKL EQ _MATKL.
  ENDMETHOD.


  METHOD GET_CUPOM.

    SELECT MAX( ITEM )
    FROM ZPMT0026
    INTO E_CUPOM.
  ENDMETHOD.


  METHOD GET_C_C_EQUIP.

    DATA(W_ORDEM) = |{ AUFNR ALPHA = IN }|.

    SELECT SINGLE *
    FROM AUFK
    INTO @DATA(W_AUFK)
     WHERE AUFNR EQ @W_ORDEM.

    IF W_AUFK-KOSTL IS INITIAL.
      KOSTL = |{ W_AUFK-CYCLE ALPHA = IN }|.
    ELSE.
      KOSTL = |{ W_AUFK-KOSTL ALPHA = IN }|.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DADOS_FATURA.

    DATA: W_FATURA TYPE ZPMT0032.
    DATA: T_FATURA TYPE TABLE OF ZPMT0024.
    DATA: TL_FATURA TYPE TABLE OF ZPMT0026.

    DATA(FATURA) = |{ I_FATURA ALPHA = IN }|.
    DATA(CNPJ)   = I_CNPJ.

    CONDENSE CNPJ.
    SELECT SINGLE *
    FROM ZPMT0032
    INTO W_FATURA
      WHERE FATURA EQ FATURA
        AND CNPJ EQ CNPJ.

    CHECK W_FATURA IS NOT INITIAL.

    DELETE FROM ZPMT0025 WHERE FATURA EQ W_FATURA-FATURA AND CNPJ EQ W_FATURA-CNPJ.
    DELETE FROM ZPMT0030 WHERE FATURA EQ W_FATURA-FATURA.
    DELETE FROM ZPMT0032 WHERE FATURA EQ W_FATURA-FATURA AND CNPJ EQ W_FATURA-CNPJ.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.


    SELECT *
    FROM ZPMT0024
    INTO TABLE T_FATURA
      WHERE FATURA EQ FATURA
        AND CNPJ EQ CNPJ.

    CHECK T_FATURA IS NOT INITIAL.
    SORT T_FATURA ASCENDING BY FATURA CUPOM_FISC.

    DELETE ZPMT0024 FROM TABLE T_FATURA.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

    SELECT *
   FROM ZPMT0026
   INTO TABLE TL_FATURA
      FOR ALL ENTRIES IN T_FATURA
     WHERE FATURA EQ T_FATURA-FATURA
       AND CUPOM_FISC EQ T_FATURA-CUPOM_FISC.
    SORT TL_FATURA ASCENDING BY FATURA CUPOM_FISC.

    DELETE ZPMT0026 FROM TABLE TL_FATURA.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

  ENDMETHOD.


  METHOD get_empresa_cfop.
    DATA: t_set    TYPE TABLE OF setleaf.

    "Verifica se existem CNPJ cadastrado, caso exista não realizar alteração no calculo CFOP.
    FREE: t_set.
    SELECT *
    FROM setleaf
    INTO TABLE t_set
       WHERE setname EQ 'MAGI_CNPJ_CFOP_ZPM0065'.

    IF i_nota IS NOT INITIAL.
      READ TABLE t_set INTO DATA(w_set) WITH KEY valfrom = i_nota-forne_cnpj.
      IF sy-subrc EQ 0.
        e_status = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method GET_FATURA.

     SELECT MAX( ITEM )
    FROM ZPMT0024
    INTO E_FATURA.


  endmethod.


  METHOD GET_IMPOSTO_ITEM_PEDIDO.

*    DATA: WA_ITE  TYPE MEPOITEM.
*    CLEAR WA_ITE.
*    CALL FUNCTION 'MEPO_DOC_ITEM_GET'
*      EXPORTING
*        IM_EBELP = I_EBELP                                    "'00010'
*      IMPORTING
*        EX_ITEM  = WA_ITE
*      EXCEPTIONS
*        FAILURE  = 1
*        OTHERS   = 2.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
**    DATA: BEGIN OF T_KONV OCCURS 0.
**            INCLUDE STRUCTURE KONV.
**          DATA: END OF T_KONV.
*
*
*
*    DATA: TY_KONV TYPE TABLE OF KOMV.
*
*    FIELD-SYMBOLS: <WMWST> TYPE ANY,
*                   <LFA1>  TYPE LFA1,
*                   <EKPO>  TYPE EKPO,
*                   <EK2>   TYPE EKPO,
*                   <EKKO>  TYPE EKKO,
*                   <VORGA> TYPE ANY,
*                   <KONV>  TYPE KONV,
*                   <CVA>   TYPE ANY.
*
*    ASSIGN ('(SAPLMEPO)ekpo') TO <EKPO>.
*    ASSIGN ('(SAPLMEPO)ekko') TO <EKKO>.
*    ASSIGN ('(SAPLMEPO)lfa1') TO <LFA1>.
*
*
*    SELECT SINGLE * FROM EKPO INTO <EKPO>
*      WHERE EBELN = I_EBELN AND
*            EBELP = I_EBELP.
*
*    IF I_NETPR GT 0.
*      <EKPO>-NETPR = I_NETPR.
*      <EKPO>-MENGE = I_MENGE.
*      <EKPO>-NETWR = ( W_MENGE * W_NETPR ) / W_PEINH.
*      <EKPO>-BRTWR = ( W_MENGE * W_NETPR ) / W_PEINH.
*      <EKPO>-EFFWR = ( W_MENGE * W_NETPR ) / W_PEINH.
*      <EKPO>-BONBA = ( W_MENGE * W_NETPR ) / W_PEINH.
*      <EKPO>-MWSKZ = I_MWSKZ.
*    ENDIF.
*
*
*    SELECT SINGLE * FROM EKKO INTO <EKKO>
*      WHERE EBELN = I_EBELN.
*
*    <EKKO>-LIFNR = I_LIFNR.
*
*    SELECT SINGLE * FROM LFA1 INTO <LFA1>
*      WHERE LIFNR = <EKKO>-LIFNR.
*
*    SELECT * FROM KONV INTO TABLE T_KONV
*      WHERE KNUMV = <EKKO>-KNUMV.
*
*    ASSIGN ('(SAPLMEPO)fc_vorga') TO <VORGA>.
*    ASSIGN ('(SAPLMEPO)cva_en') TO <CVA>.
*    ASSIGN ('(SAPLMEPO)tkomv[]') TO <KONV>.
*
*    <VORGA> = <CVA>.
*
*    PERFORM KOND_TAXES(SAPLMEPO) USING 'D' 'X'.
*
*    CHECK <EKPO>-LOEKZ = SPACE.
*    ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <WMWST>.
*
*
*    DATA: I_NETWR  TYPE KOMP-NETWR.
*
*    I_NETWR = <EKPO>-NETWR.
*    I_WMWST  =  <WMWST>.
*    I_VALOR  = ( W_NETWR + <WMWST> ).
  ENDMETHOD.


  METHOD get_nfe.

    DATA: nfe_ter TYPE REF TO zcl_nfe_inbound.
    CREATE OBJECT nfe_ter.
    DATA: t_pedido   TYPE TABLE OF ekpo,
          t_fatura   TYPE TABLE OF zpmt0032,
          t_set      TYPE TABLE OF setleaf,
          t_item_fat TYPE TABLE OF zpmt0024,
          w_item_fat TYPE zpmt0024.
    DATA: gt_pedido   TYPE TABLE OF zpmt0030,
          ws_zpmt0030 TYPE zpmt0030.
    DATA: gw_pedido TYPE zib_nfe_dist_ped.


****    Selecionando pedido sem Migo e Miro.
    SELECT *
    FROM zpmt0030
    INTO TABLE gt_pedido
      WHERE mblnr EQ space
        AND belnr EQ space
        AND canc  NE abap_true.

    CHECK  gt_pedido IS NOT INITIAL.
    SORT gt_pedido BY pedido.

    SELECT *
    FROM zpmt0032
    INTO TABLE t_fatura
      FOR ALL ENTRIES IN gt_pedido
      WHERE pedido EQ gt_pedido-pedido.

*    SELECT *
*    FROM ZPMT0024
*    INTO TABLE t_ITEM_FAT
*      FOR ALL ENTRIES IN gt_pedido
*      WHERE pedido EQ gt_pedido-pedido.

    SELECT *
    FROM zpmt0024       AS a
    INNER JOIN zpmt0026 AS b ON a~fatura     EQ b~fatura AND
                                a~cnpj       EQ b~cnpj AND
                                a~cupom_fisc EQ b~cupom_fisc
    INTO CORRESPONDING FIELDS OF TABLE t_item_fat
     FOR ALL ENTRIES IN t_fatura
   WHERE b~fatura EQ t_fatura-fatura
     AND b~cnpj   EQ t_fatura-cnpj.

    FREE: t_set.
    SELECT *
  FROM setleaf
  INTO TABLE t_set
    WHERE setname EQ 'MAGI_PM_ZPMR0055'.


    LOOP AT gt_pedido ASSIGNING FIELD-SYMBOL(<imp_pedido>).

      DATA(chave_nfe) = |{ <imp_pedido>-chave_nfe ALPHA = IN }|.
      DATA(_fatura) = |{ <imp_pedido>-fatura ALPHA = IN }|.


      "//Validando chave.
      TRY .
          nfe_ter->zif_cadastro~set_registro( i_id_registro = chave_nfe ).
          nfe_ter->set_info_sap( ).
          nfe_ter->set_coleta_tudo( i_ck_coleta_tudo = abap_true ).
          nfe_ter->ck_ignora_data_se_vencimento = abap_true.
          DATA(cabecalho) = nfe_ter->get_cabecalho_nota( ).
          DATA(lc_info) = nfe_ter->get_info_nota( ).
        CATCH zcx_cadastro INTO DATA(ex_cadastro).
          DATA(lc_msg_interna) = ex_cadastro->get_text( ).
          nfe_ter->free( ).
          CONTINUE.
        CATCH zcx_nfe_inbound_exception.
          lc_msg_interna = ex_cadastro->get_text( ).
          nfe_ter->free( ).
          CONTINUE.
        CATCH cx_sy_open_sql_db.
          lc_msg_interna = ex_cadastro->get_text( ).
          nfe_ter->free( ).
          CONTINUE.
      ENDTRY.


*&--------------------------------------------------------------------
*"//    Criar migo.

      TRY .
          LOOP AT lc_info-nfe_pedidos_alv INTO DATA(wa_pedidos_vinculados).

            gw_pedido = VALUE #(  chave_nfe = chave_nfe
                                  ebeln  = wa_pedidos_vinculados-ebeln
                                  ebelp  = wa_pedidos_vinculados-ebelp ).

            nfe_ter->add_pedido_nota( EXPORTING i_pedido = gw_pedido i_excluir = abap_true ).

          ENDLOOP.


*&--------------------------------------------------------------------
*"//    validar pedido.
          SELECT *
          FROM ekpo
          INTO TABLE t_pedido
            WHERE ebeln EQ <imp_pedido>-pedido
              AND loekz EQ ' '.

          IF t_pedido IS INITIAL.
            "Setar o cancelamento do pedido na tabela.
            zcl_webservic_protheus=>set_canc_pedido(
              i_ebeln = <imp_pedido>-pedido
            ).
          ENDIF.

          CHECK t_pedido IS NOT INITIAL.

          LOOP AT t_pedido ASSIGNING FIELD-SYMBOL(<w_pedido>) WHERE ebeln EQ <imp_pedido>-pedido.
            gw_pedido = VALUE #(  chave_nfe = chave_nfe
                                ebeln  = <w_pedido>-ebeln
                                ebelp  = <w_pedido>-ebelp
                                matnr  = <w_pedido>-matnr
                                menge  = <w_pedido>-menge
                                meins  = <w_pedido>-meins
                                netpr  = <w_pedido>-netpr
                                ).


            nfe_ter->add_pedido_nota( EXPORTING i_pedido = gw_pedido RECEIVING  r_alv = DATA(r_alv) ).
          ENDLOOP.

          "Verifica se o fornecedor pertence a regra de varias MIRO para um pedido,
          "se tiver no SET é porque se enquadra na regra, senão, processo continua normalmente.
          READ TABLE t_fatura INTO DATA(w_fatura) WITH KEY pedido = <imp_pedido>-pedido.
          IF sy-subrc EQ 0.
            READ TABLE t_set INTO DATA(w_set) WITH KEY valfrom = w_fatura-cnpj.
            IF sy-subrc EQ 0.
              nfe_ter->mod_pedido_nota( RECEIVING  r_vazio = DATA(r_vazio) ). "IR048265
              CHECK r_vazio IS INITIAL.
            ENDIF.
          ENDIF.

          "72 horas (dias úteis)
**          Verificando data de vencimento com base na codição de pgamento do pedido.
          set_data_venc_pedido( EXPORTING i_pedido         = <imp_pedido>-pedido
                                          i_dt_emissao_nfe = cabecalho-dt_emissao
                                IMPORTING e_data_venc      = DATA(data_venc) ).


          zcl_miro=>get_proximo_venc_fatura(
            IMPORTING
              e_data_vencimento = DATA(e_data_vencimento)
            EXCEPTIONS
              erro              = 1
              OTHERS            = 2
          ).

          IF e_data_vencimento GT data_venc.
            data_venc = e_data_vencimento.
          ENDIF.

          zcl_miro=>get_proximo_dia_util( EXPORTING i_data_base = data_venc
                                                    i_signum    = '+'
                                          RECEIVING r_data      = DATA(r_data) ).





*         NFE_TER->SET_DT_VENCIMENTO( I_DT_VENCIMENTO = E_DATA_VENCIMENTO ).
          nfe_ter->set_dt_vencimento( i_dt_vencimento = r_data ).
*         NFE_TER->SET_BLOQUEIO_PAGAMENTO( I_ZLSPR =  'R').
          nfe_ter->set_aceitar_documento( ).
          nfe_ter->set_aceitar_fisico( ).
          nfe_ter->set_aceitar_faturar( ).
          nfe_ter->ck_ignora_data_se_vencimento = abap_true.
          nfe_ter->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(i_gravou) ).
          DATA(lc_cabecalho) = nfe_ter->get_cabecalho_nota( ).
          nfe_ter->free( ).
        CATCH zcx_cadastro.
          nfe_ter->free( ).
        CATCH zcx_nfe_inbound_exception.
          nfe_ter->free( ).
        CATCH cx_sy_open_sql_db.
          nfe_ter->free( ).
      ENDTRY.

      TRY .

          IF lc_cabecalho-mblnr IS NOT INITIAL AND lc_cabecalho-belnr IS NOT INITIAL.
            <imp_pedido>-lifnr      = lc_cabecalho-p_emissor.
            <imp_pedido>-nfe        = lc_info-nfe_base-numero.
            <imp_pedido>-mblnr      = lc_cabecalho-mblnr.
            <imp_pedido>-belnr      = lc_cabecalho-belnr.
            <imp_pedido>-docnum_nfe = lc_cabecalho-docnum_nfe.

            READ TABLE t_item_fat INTO w_item_fat WITH KEY fatura = w_fatura-fatura
                                                           cnpj   = w_fatura-cnpj.
            IF sy-subrc EQ 0.
              w_item_fat-mblnr      = lc_cabecalho-mblnr.
              w_item_fat-belnr      = lc_cabecalho-belnr.
              w_item_fat-docnum_nfe = lc_cabecalho-docnum_nfe.
              w_item_fat-pedido     = <imp_pedido>-pedido.
              w_item_fat-CONFIRMA_RATEIO = abap_true.
              MODIFY zpmt0024 FROM w_item_fat.
            ENDIF.

*//    Gravando informações na tabela.
            MODIFY zpmt0030 FROM <imp_pedido>.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

          ELSE.
            IF lc_cabecalho-mblnr IS NOT INITIAL.
              nfe_ter->zif_cadastro~set_registro( i_id_registro = chave_nfe ).
              nfe_ter->ck_ignora_data_se_vencimento = abap_true.
              nfe_ter->nfe_inbound_cancela_fisico(  ).
            ENDIF.
          ENDIF.
        CATCH zcx_cadastro.
        CATCH zcx_nfe_inbound_exception.
        CATCH cx_sy_open_sql_db.

      ENDTRY.



      CLEAR: chave_nfe, _fatura, _fatura, lc_info.
      nfe_ter->free( ).

***      Executar rateio dos valores da ordem.
*      ZCL_WEBSERVIC_PROTHEUS=>SET_RATEIO_ZIB_CONTABIL( MBLNR = <IMP_PEDIDO>-PEDIDO ).
    ENDLOOP.

*&--------------------------------------------------------------------
*//    Gravando informações na tabela.
*    MODIFY zpmt0030 FROM TABLE gt_pedido.
*    COMMIT WORK.
  ENDMETHOD.


  METHOD GET_ORDEM.

    DATA:T_USERMD            TYPE STANDARD TABLE OF  RGSB4.
    CLEAR: T_USERMD.

*    BUSCANDO EMPRESA TABELA SETLEAF.
    SELECT SINGLE *
    FROM SETLEAF
    INTO @DATA(ORDEM)
      WHERE SETNAME EQ 'MAGGI_ZPM0065'
        AND VALFROM EQ @WERKS.

    CHECK ORDEM IS NOT INITIAL.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        CLASS           = '0000'
        SETNR           = 'MAGGI_ZPM0065'
        NO_DESCRIPTIONS = ' '
      TABLES
        SET_VALUES      = T_USERMD
      EXCEPTIONS
        SET_NOT_FOUND   = 1
        OTHERS          = 2.

    IF T_USERMD IS NOT INITIAL.
      READ TABLE T_USERMD INTO DATA(WA_USERMD) WITH KEY FROM = WERKS.
      AUFNR = WA_USERMD-TITLE.
      AUFNR = |{ AUFNR ALPHA = IN }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_status_pedido.
    DATA: gt_zpmt0030 TYPE TABLE OF zpmt0030,
          t_zpmt0030  TYPE TABLE OF zpmt0030.

    SELECT SINGLE *
    FROM zpmt0032
    INTO @DATA(w_zpmt0032)
      WHERE fatura EQ @i_fatura
          AND cnpj EQ @cnpj.

    CHECK w_zpmt0032 IS NOT INITIAL.

    SELECT SINGLE procstat
    FROM ekko
    INTO i_status
      WHERE ebeln EQ w_zpmt0032-pedido.

    CHECK i_status IS NOT INITIAL.


    zcl_webservic_protheus=>set_satus_pedido( EXPORTING
                                             i_fatura = i_fatura
                                             i_cnpj   = cnpj
                                             i_status = i_status ).

    "Verificar se a fatura tem NFe.
    IF i_status EQ '05'.
      SELECT *
      FROM zpmt0024
      INTO TABLE @DATA(t_zpmt0024)
      WHERE fatura EQ @i_fatura
          AND fatura EQ @w_zpmt0032-fatura
          AND cnpj EQ @cnpj
          AND chave_nfe NE ' '.

      IF t_zpmt0024 IS NOT INITIAL.

        "Verificar se a fatura e pedido ja existe, se existir não precisa salvar novamente.
        SELECT *
        FROM zpmt0030
        INTO TABLE t_zpmt0030
        WHERE pedido EQ w_zpmt0032-pedido AND fatura EQ w_zpmt0032-fatura.

        IF sy-subrc NE 0.
"*---> 28/06/2023 - Migração S4 - LO
          SORT t_zpmt0024 by cnpj fatura chave_nfe.
"*---> 28/06/2023 - Migração S4 - LO
          DELETE ADJACENT DUPLICATES FROM t_zpmt0024 COMPARING cnpj fatura chave_nfe.
          LOOP AT t_zpmt0024 ASSIGNING FIELD-SYMBOL(<w_zpmt0024>).
*          READ TABLE t_zpmt0030 INTO DATA(ws_zpmt0030) WITH KEY chave_nfe = <w_zpmt0024>-chave_nfe.
*          IF ws_zpmt0030-belnr IS INITIAL OR ws_zpmt0030-canc IS INITIAL.
            APPEND VALUE #( chave_nfe = <w_zpmt0024>-chave_nfe
                            fatura    = w_zpmt0032-fatura
                            pedido    = w_zpmt0032-pedido
                                      ) TO gt_zpmt0030.
*          ENDIF.
*          CLEAR: ws_zpmt0030.
          ENDLOOP.

          MODIFY zpmt0030 FROM TABLE gt_zpmt0030.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

        ENDIF.
      ENDIF.


**   "Cria o JOB para processar os dados da nota fiscal, criar migo e miro.
*          CALL METHOD zcl_webservic_protheus=>call_report
*            EXPORTING
*              i_sequen = CONV #( |{ w_zpmt0032-pedido ALPHA = OUT }| )
*              i_report = 'ZPMR0055'.
*          EXIT.
    ENDIF.


  ENDMETHOD.


  METHOD post_fatura.


    DATA: p_erro TYPE p DECIMALS 2.
    DATA: p_fatura TYPE re_belnr.
    DATA: gw_veiculo TYPE equz.
    DATA: t_fatura TYPE TABLE OF zpmt0032.
    DATA: gt_fatura TYPE TABLE OF zpmt0024.
    DATA: gt_cupom TYPE TABLE OF zpmt0026.
    DATA: gt_return TYPE TABLE OF zpme0036.
    DATA: t_return TYPE TABLE OF zpme0036.
    DATA: cont_cupom TYPE p DECIMALS 2.
    DATA: idcupom TYPE p DECIMALS 2.
    DATA: idmat TYPE p DECIMALS 2.
    DATA: idnfe TYPE p DECIMALS 2.
    DATA: wc_json            TYPE string.
    DATA: nfe_ter TYPE REF TO zcl_nfe_inbound,
          s_cnpj  TYPE stcd1,
          s_ie    TYPE stcd3.
    CREATE OBJECT nfe_ter.

    DATA: i_ordem         TYPE aufnr,
          wa_return       TYPE bapiret2,
          wa_data_general TYPE bapi_itob.

* Verificando sequencia do cupom e do material.
    CLEAR: idcupom, idmat.
    zcl_webservic_protheus=>get_cupom( RECEIVING e_cupom = idmat ).
    zcl_webservic_protheus=>get_fatura( RECEIVING e_fatura = idcupom ).

*&--------------------------------------------------------------------
****Recebendo informações.
    LOOP AT i_fatura ASSIGNING FIELD-SYMBOL(<imp_fatura>).

*        Verificando se existe fatura.
      IF <imp_fatura>-fatura IS NOT INITIAL.
        zcl_webservic_protheus=>get_dados_fatura(
           EXPORTING
          i_fatura = <imp_fatura>-fatura
          i_cnpj   = <imp_fatura>-cnpj ).
      ENDIF.

      LOOP AT <imp_fatura>-dados ASSIGNING FIELD-SYMBOL(<im_cupom>).

*    Verificando se existe correção de fatura.
        IF <imp_fatura>-fat_ant IS NOT INITIAL.
          zcl_webservic_protheus=>cons_fatura(
             EXPORTING
            i_fatura = <imp_fatura>-fat_ant
            i_cupom  = <im_cupom>-cupom_fisc ).
        ENDIF.

**      Check placa veiculo.
*        CLEAR: gw_veiculo.
*        SELECT SINGLE *
*          FROM equz AS a
*          INNER JOIN equi AS b ON b~equnr EQ a~equnr
*          INNER JOIN fleet AS c ON c~objnr EQ b~objnr
*            INTO CORRESPONDING FIELDS OF gw_veiculo
*              WHERE c~license_num EQ <im_cupom>-placa
*                AND a~datbi EQ '99991231'.

*      Check placa veiculo.
        CLEAR: gw_veiculo.
        SELECT SINGLE *
          FROM fleet AS a
          INNER JOIN equi AS b ON b~objnr EQ a~objnr
          INNER JOIN equz AS c ON c~equnr EQ b~equnr
            INTO CORRESPONDING FIELDS OF gw_veiculo
              WHERE a~license_num EQ <im_cupom>-placa
                AND b~eqtyp EQ 'A'
                AND c~datbi EQ '99991231'.


*&--------------------------------------------------------------------
* Processando dados da fatura.
        IF  gw_veiculo IS NOT INITIAL.
          CLEAR: s_cnpj, s_ie.
          s_cnpj = <imp_fatura>-cnpj.
          s_ie   = <imp_fatura>-ie.

* Consultando fornecedor.
          REPLACE '.' IN s_cnpj WITH ''.
          REPLACE '-' IN s_cnpj WITH ''.
          REPLACE ',' IN s_cnpj WITH ''.

          REPLACE '.' IN s_ie  WITH ''.
          REPLACE '.' IN s_ie  WITH ''.
          REPLACE '-' IN s_ie  WITH ''.
          REPLACE ',' IN s_ie  WITH ''.

          CALL METHOD zcl_webservic_protheus=>sel_fornecedor
            EXPORTING
              i_cnpj    = CONV #( s_cnpj )
              i_ie      = s_ie
              i_empresa = CONV #( abap_false )
            IMPORTING
              e_lifnr   = DATA(lifnr).

          IF  lifnr IS NOT INITIAL.

            "Caso a fatura tenha NFe fazer a validação.
            IF <im_cupom>-chave_nfe IS NOT INITIAL.
              TRY .
                  nfe_ter->zif_cadastro~set_registro( i_id_registro = <im_cupom>-chave_nfe ).
                  nfe_ter->set_info_sap( ).
                  nfe_ter->set_coleta_tudo( i_ck_coleta_tudo = abap_true ).
                  nfe_ter->ck_ignora_data_se_vencimento = abap_true.
                  DATA(cabecalho) = nfe_ter->get_cabecalho_nota( ).
                  DATA(lc_info) = nfe_ter->get_info_nota( ).
                CATCH zcx_cadastro INTO DATA(ex_cadastro).
                  DATA(lc_msg_interna) = ex_cadastro->get_text( ).
                  nfe_ter->free( ).
              ENDTRY.

              IF lc_info-nfe_base-numero IS NOT INITIAL.


              ELSE.

                APPEND VALUE #( e_cod_retorn = '999'
                    e_fatura   = |{ <imp_fatura>-fatura ALPHA = IN }|
                        cupom  = VALUE #( (
                 e_desc_retorn = 'Erro! chave -> ' && <im_cupom>-chave_nfe && ' não encontrado'
                    e_cupom    =  |{ <im_cupom>-cupom_fisc ALPHA = IN }|
                                  )  ) ) TO gt_return.

                CONTINUE.
              ENDIF.
            ENDIF.


            ADD 1 TO idcupom.

            APPEND VALUE  #(
                             item          = idcupom
                             cnpj          = <imp_fatura>-cnpj
                             lifnr         = lifnr
                             fatura        =  |{ <imp_fatura>-fatura ALPHA = IN }|
                             empresa       = <imp_fatura>-empresa
                             dt_fatura     = <imp_fatura>-dt_fatura
                             hr_fatura     = <imp_fatura>-hr_fatura
                             vlr_fatura    = <imp_fatura>-vlr_fatura
                             cliente       = <imp_fatura>-cliente
                             cnpj_cliente  = <imp_fatura>-cnpj_cliente
                             fat_ant       = <imp_fatura>-fat_ant
                             centro        = gw_veiculo-iwerk
                             chave_nfe     = <im_cupom>-chave_nfe
                             cupom_fisc    = |{ <im_cupom>-cupom_fisc ALPHA = IN }|
                             serie_cupom   = |{ <im_cupom>-serie_cupom ALPHA = IN }|
                             dt_cupom_fisc = <im_cupom>-dt_cupom_fisc
                             hr_cupom_fisc = <im_cupom>-hr_cupom_fisc
                             dt_exportacao = <im_cupom>-dt_exportacao
                             hr_exportacao = <im_cupom>-hr_exportacao
                             placa         = <im_cupom>-placa
                             odometro      = <im_cupom>-odometro
                             ordem         = <im_cupom>-ordem
                             equnr         = gw_veiculo-equnr
                            ) TO gt_fatura.
            CLEAR: lifnr.
*&--------------------------------------------------------------------
* Processando dados do cupom.
            LOOP AT <im_cupom>-material ASSIGNING FIELD-SYMBOL(<_material>).

*&--------------------------------------------------------------------
* Adicionando sequencia do cupom gravado na tabela.
              ADD 1 TO idmat.

              APPEND VALUE #(
                                item           = idmat
                                cnpj           = <imp_fatura>-cnpj
                                fatura         = |{ <imp_fatura>-fatura ALPHA = IN }|
                                chave_nfe     =     <im_cupom>-chave_nfe
                                cupom_fisc     = |{ <im_cupom>-cupom_fisc ALPHA = IN }|
                                cod_material   = |{ <_material>-cod_material ALPHA = IN }|
                                desc_material  = <_material>-desc_material
                                und            = <_material>-und
                                qtde           = <_material>-qtde
                                vlr_unt        = <_material>-vlr_unt
                                vlr_total      = <_material>-vlr_total
                            ) TO gt_cupom.

*&--------------------------------------------------------------------
* Registrando o retorno do processamento.
              APPEND VALUE #(   e_cod_retorn  = '0'
                                e_fatura      = |{ <imp_fatura>-fatura ALPHA = IN }|
                                e_desc_retorn = |Dados da fatura -> { <imp_fatura>-fatura } salvo com sucesso.|
                            )  TO t_return.




            ENDLOOP.
          ELSE.

*&--------------------------------------------------------------------
*       Registrando erro.
            APPEND VALUE #(  e_cod_retorn = '999'
                             e_fatura   = |{ <imp_fatura>-fatura ALPHA = IN }|
                             cupom  = VALUE #( (
                             e_desc_retorn = 'Fornecedor-> ' && <imp_fatura>-empresa && ' não cadastrado ou desativado na base de dados do SAP.'
                             e_cupom    =  |{ <im_cupom>-cupom_fisc ALPHA = IN }| ) )
                          ) TO gt_return.
          ENDIF.
        ELSE.

*&--------------------------------------------------------------------
*       Registrando erro.
          APPEND VALUE #(  e_cod_retorn = '999'
                           e_fatura   = |{ <imp_fatura>-fatura ALPHA = IN }|
                           cupom  = VALUE #( (
                           e_desc_retorn = 'Veiculo-> ' && <im_cupom>-placa && ' não cadastrado na base de dados do SAP.'
                           e_cupom    =  |{ <im_cupom>-cupom_fisc ALPHA = IN }| ) )
                         )  TO gt_return.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

*&--------------------------------------------------------------------------------------------
*  Verificar se existe fatura com problema.
    DATA: r_fatura TYPE RANGE OF j_objnr.
    FREE: r_fatura.
    r_fatura =  VALUE #( FOR ls IN gt_return FOR lw IN gt_fatura WHERE ( fatura EQ ls-e_fatura )
                       ( sign = 'I' option = 'EQ' low = lw-fatura ) ).
    SORT r_fatura.
    SORT gt_fatura.
    IF r_fatura IS NOT INITIAL.
      DELETE gt_fatura[] WHERE fatura IN r_fatura.
      DELETE gt_cupom[] WHERE fatura IN r_fatura.
    ENDIF.

*&--------------------------------------------------------------------------------------------
*  Verificar se existe CUPOM com problema.
    FREE: r_fatura.
    r_fatura =  VALUE #( FOR ls IN gt_return FOR lt IN gt_cupom WHERE ( fatura EQ ls-e_fatura )
                       ( sign = 'I' option = 'EQ' low = lt-fatura ) ).

    SORT r_fatura.
    SORT gt_fatura.
    IF r_fatura IS NOT INITIAL.
      DELETE gt_cupom[] WHERE fatura IN r_fatura.
    ENDIF.

*&--------------------------------------------------------------------
*Aplicando cabeçalho da fatura.
    MOVE-CORRESPONDING gt_fatura TO t_fatura.
    DELETE ADJACENT DUPLICATES FROM t_fatura COMPARING fatura.

*&--------------------------------------------------------------------
* Aplicando as tabelas internas na Fisica
    MODIFY zpmt0032 FROM TABLE t_fatura.
    MODIFY zpmt0024 FROM TABLE gt_fatura.
    MODIFY zpmt0026 FROM TABLE gt_cupom.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*&--------------------------------------------------------------------
* "//Verificando as fatura se tem cupom com erro.
    FREE: r_fatura.
    SORT t_return ASCENDING BY e_fatura.
    DELETE ADJACENT DUPLICATES FROM t_return COMPARING e_fatura.
    r_fatura =  VALUE #( FOR ts IN gt_return FOR tl IN t_return WHERE ( e_fatura EQ ts-e_fatura )
                       ( sign = 'I' option = 'EQ' low = tl-e_fatura ) ).

    SORT t_return.
    IF r_fatura IS NOT INITIAL.
      DELETE t_return[] WHERE e_fatura IN r_fatura.
    ENDIF.
    APPEND LINES OF t_return[] TO gt_return.

*&--------------------------------------------------------------------
* "//Retornando mensagem.
    SORT gt_return ASCENDING BY e_fatura.
    CLEAR: p_fatura.
    LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<gw_retorn>).

      IF p_fatura NE <gw_retorn>-e_fatura.
        APPEND  VALUE #(  e_cod_retorn     = <gw_retorn>-e_cod_retorn
                        e_fatura         = <gw_retorn>-e_fatura
                        e_desc_retorn    = <gw_retorn>-e_desc_retorn ) TO e_fatura.
      ENDIF.

      LOOP AT <gw_retorn>-cupom ASSIGNING FIELD-SYMBOL(<gs_retorn>).
        LOOP AT e_fatura ASSIGNING FIELD-SYMBOL(<ls_return>).
          APPEND VALUE #( e_desc_retorn = <gs_retorn>-e_desc_retorn
                               e_cupom  = <gs_retorn>-e_cupom       ) TO <ls_return>-cupom.
        ENDLOOP.
      ENDLOOP.

      p_fatura = <gw_retorn>-e_fatura.
    ENDLOOP.
  ENDMETHOD.


  METHOD SEL_FORNECEDOR.

    CLEAR: E_LIFNR.

    TRY .
        ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
          )->SET_PARCEIRO_CNPJ_CPF_IE(
          EXPORTING
            I_CNPJ             = I_CNPJ    " Code CGC
            I_INSC_ESTATUAL    = I_IE" Nº identificação fiscal 3
          )->CK_ATIVO_EMPRESA( I_EMPRESA = I_EMPRESA
          )->GET_ID_PARCEIRO( IMPORTING E_PARCEIRO = E_LIFNR    " Identificação do parceiro (cliente, fornecedor, loc.negócio)
          ).
*          CATCH ZCX_PARCEIROS.    " .

      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
        EX_PARCEIROS->PUBLISHED_ERRO(
          EXPORTING
            I_MSGTY         = 'S'    " Campo do sistema: tipo de mensagem
            I_MSGTY_DISPLAY = 'E'    " Campo do sistema: tipo de mensagem

        ).
    ENDTRY.

  ENDMETHOD.


  METHOD SERVER.

    DATA OEXCP        TYPE REF TO CX_ROOT.
    DATA FUNCTION     TYPE RS38L_FNAM.
    DATA EXCEPTHEADER TYPE STRING.
    DATA PARAMS       TYPE ZRFC_FINT_P_T.
    DATA PARAMTAB     TYPE ABAP_FUNC_PARMBIND_TAB.
    DATA EXCEPTAB     TYPE ABAP_FUNC_EXCPBIND_TAB.
    DATA OPEN_KEY     TYPE CHAR1 VALUE '{'.
    DATA CLOSE_KEY    TYPE CHAR1 VALUE '}'.

    DATA(I_CDATA) = REQUEST->GET_CDATA( ).

    FUNCTION = SWITCH #( VIEW WHEN 'PM_PROTHEUS'  THEN 'ZPM_IMP_FATURA_DO_PROTHEUS' ).

    CALL METHOD ZCL_FMCALL_HANDLER=>BUILD_PARAMS
      EXPORTING
        FUNCTION_NAME    = FUNCTION
      IMPORTING
        PARAMS           = PARAMS
        PARAMTAB         = PARAMTAB
        EXCEPTAB         = EXCEPTAB
      EXCEPTIONS
        INVALID_FUNCTION = 1
        OTHERS           = 2.

    DATA(_PARAMETER) = PARAMS[ PARAMCLASS = 'I' EXID = 'h' ]-PARAMETER.
    DATA(_CDATA) = |{ OPEN_KEY } "{ _PARAMETER }": { I_CDATA } { CLOSE_KEY }|.

    TRY.
        CALL METHOD ZCL_FMCALL_PM_MOBILE=>JSON_DESERIALIZE
          EXPORTING
            JSON     = _CDATA
          CHANGING
            PARAMTAB = PARAMTAB.
      CATCH CX_ROOT INTO OEXCP.
        DATA(ETEXT) = OEXCP->IF_MESSAGE~GET_TEXT( ).
    ENDTRY.

    TRY.
        CALL FUNCTION FUNCTION
          PARAMETER-TABLE
          PARAMTAB
          EXCEPTION-TABLE
          EXCEPTAB.
      CATCH CX_ROOT INTO OEXCP.
        ETEXT = OEXCP->IF_MESSAGE~GET_LONGTEXT(  PRESERVE_NEWLINES = ABAP_TRUE ).
    ENDTRY.

    DATA(FUNCRC) = SY-SUBRC.
    DELETE EXCEPTAB WHERE VALUE NE FUNCRC.

    IF LINE_EXISTS( EXCEPTAB[ VALUE = FUNCRC ] ).

      DATA(EXCEPTION) = EXCEPTAB[ VALUE = FUNCRC ].

      EXCEPTHEADER = EXCEPTION-NAME.

      CALL METHOD RESPONSE->SET_HEADER_FIELD(
          NAME  = 'X-SAPRFC-Exception'
          VALUE = EXCEPTHEADER ).

    ENDIF.

    CALL METHOD ZCL_FMCALL_PM_MOBILE=>SERIALIZE_JSON
      EXPORTING
        PARAMTAB  = PARAMTAB
        EXCEPTAB  = EXCEPTAB
        PARAMS    = PARAMS
        LOWERCASE = ABAP_TRUE
      IMPORTING
        O_STRING  = DATA.




  ENDMETHOD.


  METHOD SET_CANC_PEDIDO.

    DATA: W_ZPMT0030 TYPE ZPMT0030.

    SELECT SINGLE * FROM ZPMT0030 INTO W_ZPMT0030 WHERE PEDIDO = I_EBELN.

    W_ZPMT0030-CANC = 'X'.

    MODIFY ZPMT0030 FROM W_ZPMT0030.
    COMMIT WORK.
    CLEAR: W_ZPMT0030.
  ENDMETHOD.


  METHOD SET_DATA_VENC_PEDIDO.

    SELECT SINGLE *
    FROM EKKO
    INTO @DATA(WA_EKKO)
      WHERE EBELN EQ @I_PEDIDO.

    CHECK WA_EKKO IS NOT INITIAL.

    CALL FUNCTION 'NET_DUE_DATE_GET'
      EXPORTING
        I_ZFBDT = I_DT_EMISSAO_NFE
        I_ZBD1T = WA_EKKO-ZBD1T
        I_ZBD2T = WA_EKKO-ZBD2T
        I_ZBD3T = WA_EKKO-ZBD3T
        I_SHKZG = SPACE
        I_REBZG = SPACE
        I_KOART = SPACE
      IMPORTING
        E_FAEDT = E_DATA_VENC.

  ENDMETHOD.


  METHOD SET_PEDIDO.

******Topo****************.
    TYPES:

      BEGIN OF TY_FIELDS,
        CAMPO(30) TYPE C,
        GROUP1(5) TYPE C,
        VALUE     TYPE SY-TABIX,
        INVISIBLE TYPE SY-TABIX,
      END   OF TY_FIELDS,

      BEGIN OF TY_EDITOR,
        LINE(72),
      END   OF TY_EDITOR.

    DATA:
      IT_EDITOR_PED             TYPE STANDARD TABLE OF TY_EDITOR,  "Tabela para extração do texto da solicitação de compra
      WA_EDITOR_PED             TYPE TY_EDITOR,
      IT_RETURN_PED             TYPE STANDARD TABLE OF BAPIRET2, "TABLE OF BAPIRET2 WITH HEADER LINE,
      WA_RETURN_PED             TYPE BAPIRET2,
      IT_POITEM_PED             TYPE STANDARD TABLE OF BAPIMEPOITEM, "TABLE OF BAPIMEPOITEM WITH HEADER LINE,
      WA_POITEM_PED             TYPE BAPIMEPOITEM,
      IT_POITEMX_PED            TYPE STANDARD TABLE OF BAPIMEPOITEMX,
      WA_POITEMX_PED            TYPE BAPIMEPOITEMX,
      WA_POHEADER_PED           TYPE BAPIMEPOHEADER,
      WA_POHEADERX_PED          TYPE BAPIMEPOHEADERX,
      IT_POACCOUNT              TYPE STANDARD TABLE OF BAPIMEPOACCOUNT,
      IT_POACCOUNTX             TYPE STANDARD TABLE OF BAPIMEPOACCOUNTX,
      IT_BAPIMEPOTEXTHEADER_PED TYPE STANDARD TABLE OF BAPIMEPOTEXTHEADER,
      WA_BAPIMEPOTEXTHEADER_PED TYPE BAPIMEPOTEXTHEADER,
      IT_POSCHEDULE             TYPE STANDARD TABLE OF  BAPIMEPOSCHEDULE,
      IT_POSCHEDULEX            TYPE STANDARD TABLE OF  BAPIMEPOSCHEDULX,
      PURCHASEORDER             TYPE BAPIMEPOHEADER-PO_NUMBER,
      V_EKORG                   TYPE T024W-EKORG,
      V_BRANCH                  TYPE T001W-J_1BBRANCH,
      V_BUKRS                   TYPE J_1BBRANCH-BUKRS,
      V_LIFNR                   TYPE LFA1-LIFNR,
      W_ZMMT0035                TYPE ZMMT0035.



    DATA: WA_MAKT     TYPE MAKT,
          WA_MARA     TYPE MARA,
          OBG_DESCBOX TYPE REF TO CL_GUI_TEXTEDIT.

    DATA: IT_RETURN TYPE STANDARD TABLE OF BAPIRETURN,
          WA_RETURN TYPE BAPIRETURN.

    REFRESH: IT_RETURN_PED, IT_POITEM_PED, IT_POITEMX_PED,
    IT_BAPIMEPOTEXTHEADER_PED.
    CLEAR: WA_RETURN_PED, WA_POITEM_PED, WA_POITEMX_PED,
           WA_POHEADER_PED, WA_POHEADERX_PED, WA_BAPIMEPOTEXTHEADER_PED.


    MOVE-CORRESPONDING T_OBS[] TO IT_EDITOR_PED[].

******************Processando dados********************************************
    LOOP AT T_FATURA INTO DATA(I_FATURA).

***   Selecionando descrição do material
      SELECT SINGLE * INTO WA_MAKT FROM MAKT WHERE MATNR EQ I_FATURA-MATNR AND SPRAS EQ SY-LANGU.


*&------------------------------------------------------------------------------------------------------
*Corpo do pedido.
"*---> 28/06/2023 - Migração S4 - LO
*      APPEND VALUE #( PO_ITEM    = I_FATURA-ITEM                     "Item
*                      ACCTASSCAT = I_FATURA-ACCTASSCAT               "Categoria de classificação contábil
*                      MATERIAL   = I_FATURA-MATNR                    "Material
*                      QUANTITY   = I_FATURA-QTDE                     "Quantidade
*                      PO_PRICE   = 1                                 "Transferência do preço: 1 = bruto, 2 = líquido
*                      NET_PRICE  = I_FATURA-VLR_UNT                  "Preço
*                      PRICE_UNIT = I_FATURA-PRICE_UNIT
*                      TAX_CODE   = I_FATURA-TAX_CODE                 "Código do Imposto
*                      PLANT      = I_FATURA-CENTRO                    "Centro
*                                 ) TO IT_POITEM_PED.
   DATA(v_len) = strlen( I_FATURA-MATNR ).

   IF v_len > 18.
      APPEND VALUE #( PO_ITEM         = I_FATURA-ITEM                     "Item
                      ACCTASSCAT      = I_FATURA-ACCTASSCAT               "Categoria de classificação contábil
                      MATERIAL_LONG   = I_FATURA-MATNR                    "Material
                      QUANTITY        = I_FATURA-QTDE                     "Quantidade
                      PO_PRICE        = 1                                 "Transferência do preço: 1 = bruto, 2 = líquido
                      NET_PRICE       = I_FATURA-VLR_UNT                  "Preço
                      PRICE_UNIT      = I_FATURA-PRICE_UNIT
                      TAX_CODE        = I_FATURA-TAX_CODE                 "Código do Imposto
                      PLANT           = I_FATURA-CENTRO                    "Centro
                                      ) TO IT_POITEM_PED.
   ELSE.
      APPEND VALUE #( PO_ITEM    = I_FATURA-ITEM                     "Item
                      ACCTASSCAT = I_FATURA-ACCTASSCAT               "Categoria de classificação contábil
                      MATERIAL   = I_FATURA-MATNR                    "Material
                      QUANTITY   = I_FATURA-QTDE                     "Quantidade
                      PO_PRICE   = 1                                 "Transferência do preço: 1 = bruto, 2 = líquido
                      NET_PRICE  = I_FATURA-VLR_UNT                  "Preço
                      PRICE_UNIT = I_FATURA-PRICE_UNIT
                      TAX_CODE   = I_FATURA-TAX_CODE                 "Código do Imposto
                      PLANT      = I_FATURA-CENTRO                    "Centro
                                 ) TO IT_POITEM_PED.
   ENDIF.
"*---> 28/06/2023 - Migração S4 - LO

      APPEND VALUE #( PO_ITEM    = I_FATURA-ITEM                     "Item
                      ACCTASSCAT = 'X'                               "Categoria de classificação contábil
                      PO_ITEMX   = 'X'                               "Item
                      MATERIAL   = 'X'                               "Material
                      QUANTITY   = 'X'                               "Quantidade
                      PO_PRICE   = 'X'                               "Transferência do preço: 1 = bruto, 2 = líquido
                      NET_PRICE  = 'X'
                      PRICE_UNIT = 'X'                              "Preço
                      TAX_CODE   = 'X'                               "Código do Imposto
                      PLANT      = 'X'                               "Centro*
                                 ) TO IT_POITEMX_PED.

      APPEND VALUE #( PO_ITEM  = I_FATURA-ITEM
                      ORDERID  = I_FATURA-ORDEM
*                      ACTIVITY = I_FATURA-ACTIVITY                  " Operação da ordem
                   GL_ACCOUNT  = I_FATURA-GL_ACCOUNT                "Conta contabil ( exemplo = '0000412012' )
                   COSTCENTER  = I_FATURA-COSTCENTER                "Centro de custo
                   TAX_CODE    = I_FATURA-TAX_CODE                  "Cod do imposto
                   CO_AREA     = I_FATURA-CO_AREA                   "Area contabil
                   CREAT_DATE  = SY-DATUM
                   QUANTITY    = I_FATURA-QTDE
                    ) TO IT_POACCOUNT.

      APPEND VALUE #( PO_ITEM  = I_FATURA-ITEM
                      PO_ITEMX = 'X'
                      ORDERID  = 'X'
                      ACTIVITY = 'X'
                   GL_ACCOUNT  = 'X'
                   COSTCENTER  = 'X'
                   TAX_CODE    = 'X'
                   CO_AREA     = 'X'
                   CREAT_DATE  = 'X'
                   QUANTITY    = 'X'
                    ) TO IT_POACCOUNTX.


*&------------------------------------------------------------------------------------------------------
*Dados da remessa.
      APPEND VALUE #( PO_ITEM       = I_FATURA-ITEM
                     DELIVERY_DATE  = I_FATURA-DATA_REMESSA ) TO IT_POSCHEDULE.

      APPEND VALUE #( PO_ITEM        = I_FATURA-ITEM
                      DELIVERY_DATE  = 'X' ) TO IT_POSCHEDULEX.

*&------------------------------------------------------------------------------------------------------
      "Cabeçalho----------
      SELECT SINGLE J_1BBRANCH
        FROM T001W
        INTO V_BRANCH
        WHERE WERKS EQ I_FATURA-CENTRO.

      SELECT SINGLE BUKRS
        FROM J_1BBRANCH
        INTO V_BUKRS
        WHERE BRANCH EQ V_BRANCH.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = I_FATURA-LIFNR
*        IMPORTING
*          OUTPUT = V_LIFNR.

*      WA_POHEADER_PED-VENDOR   = I_FATURA-LIFNR.                   "Fornecedor pela ZMM0045

      SELECT SINGLE EKORG
          FROM T024W
          INTO V_EKORG
          WHERE WERKS EQ I_FATURA-CENTRO.

*&------------------------------------------------------------------------------------------------------
*Cabeçalho.
      WA_POHEADER_PED = VALUE #( PMNTTRMS  = I_FATURA-ZTERM          "Chave condições pagamento
*                                DSCNT1_TO = '05'                    "Dias de desconto
                                 VENDOR    = I_FATURA-LIFNR
                                 COMP_CODE = V_BUKRS                 "Empresa
                                 DOC_TYPE  = I_FATURA-BSART          "Tipo de Pedido
                                 PURCH_ORG = V_EKORG                 "Organização de Compras
                                 DOC_DATE  = SY-DATUM                "Data do Pedido
                                 LANGU     = SY-LANGU                "Idioma
                                 PUR_GROUP = I_FATURA-EKGRP          "Grupo de Compradores
                                 CURRENCY  = 'BRL' ).                "Moeda pela ZMM0045

      WA_POHEADERX_PED = VALUE #( PMNTTRMS  = 'X'
                                  COMP_CODE = 'X'                     "Empresa
                                  DOC_TYPE  = 'X'                     "Tipo de Pedido
                                  VENDOR    = 'X'                     "Fornecedor pela ZMM0045
                                  PURCH_ORG = 'X'                     "Organização de Compras
                                  DOC_DATE  = 'X'                     "Data do Pedido
                                  LANGU     = 'X'                     "Idioma
                                  PUR_GROUP = 'X' ).                  "Grupo de Compradores
    ENDLOOP.


    "Texto Cabeçalho----
    IF OBG_DESCBOX IS NOT INITIAL.
      CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
        IMPORTING
          TABLE = IT_EDITOR_PED.
    ENDIF.


    LOOP AT IT_EDITOR_PED INTO WA_EDITOR_PED.
      APPEND VALUE #( TEXT_ID   = 'F01'
                      TEXT_FORM = '*'
                      TEXT_LINE = WA_EDITOR_PED-LINE ) TO IT_BAPIMEPOTEXTHEADER_PED.
    ENDLOOP.

"*---> 28/06/2023 - Migração S4 - LO
    CALL FUNCTION 'BAPI_PO_CREATE1'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        POHEADER         = WA_POHEADER_PED
        POHEADERX        = WA_POHEADERX_PED
      IMPORTING
        EXPPURCHASEORDER = PURCHASEORDER
      TABLES
        RETURN           = E_RETURNG
        POITEM           = IT_POITEM_PED
        POACCOUNT        = IT_POACCOUNT
        POACCOUNTX       = IT_POACCOUNTX
        POITEMX          = IT_POITEMX_PED
        POTEXTHEADER     = IT_BAPIMEPOTEXTHEADER_PED
        POSCHEDULE       = IT_POSCHEDULE
        POSCHEDULEX      = IT_POSCHEDULEX.

    READ TABLE E_RETURNG INTO WA_RETURN_PED WITH KEY TYPE = 'S' ID = '06' NUMBER = '017'.
    IF SY-SUBRC EQ 0.
      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
      MESSAGE S000(ZWRM001) DISPLAY LIKE 'S' WITH E_MESSAGE.
      E_MESSAGE = WA_RETURN_PED-MESSAGE.
      E_PEDIDO = PURCHASEORDER.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      SORT E_RETURNG BY NUMBER DESCENDING.
      READ TABLE E_RETURNG INTO WA_RETURN_PED WITH KEY TYPE = 'E'.
      E_MESSAGE = WA_RETURN_PED-MESSAGE.
*      MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH E_MESSAGE.
    ENDIF.

    FREE: IT_POITEM_PED, IT_POACCOUNT, IT_POACCOUNTX, IT_POITEMX_PED, IT_BAPIMEPOTEXTHEADER_PED, IT_POSCHEDULE, IT_POSCHEDULEX.
    CLEAR: WA_POHEADER_PED, WA_POHEADERX_PED.
  ENDMETHOD.


  METHOD set_rateio_zib_contabil.

    DATA: z_obj_key TYPE awkey.
    DATA: w_obj_key TYPE char13.
    DATA z_ano_chav TYPE char04.
    DATA z_cod_chav TYPE char03.
    DATA z_ano_atual TYPE char04.
*---> 20.06.2023 - Migração S4 - DG
"    DATA: z_seqitem TYPE char02.
    DATA: Z_SEQITEM TYPE ZCHAR02.
*<--- 20.06.2023 - Migração S4 - DG
    DATA: z_vlr_item TYPE dmbtr.
    DATA: z_vlr_abast TYPE dmbtr.
    DATA: z_vlr_dolar TYPE dmbe2.
    DATA: dif_vlr TYPE p DECIMALS 2.
    DATA: dif_vlr_dol TYPE p DECIMALS 2.
    DATA: z_tot_item TYPE p DECIMALS 2.
    DATA: gt_zpmt0030 TYPE TABLE OF zpmt0030.
    DATA: zdzuonr TYPE dzuonr.
    DATA: gt_zib_contabil TYPE TABLE OF zib_contabil.
    DATA: gt_zpmt0063 TYPE TABLE OF zpmt0063.
    DATA: gw_zib_contabil TYPE zib_contabil.
    DATA: gw_zpmt0063 TYPE zpmt0063.
    DATA: r_chav_ref TYPE RANGE OF bkpf-awkey.
    DATA: t_ekpo TYPE TABLE OF ekpo.
    DATA: t_ekko TYPE TABLE OF ekko.
    DATA: t_COBL TYPE TABLE OF COBL.
    DATA: gt_abastecimentos  TYPE TABLE OF ZPMT0026.
    DATA: gt_dados_cupons  TYPE TABLE OF ZPMT0024.
    DATA: gt_dados_cupons_aux  TYPE TABLE OF ZPMT0024.
    DATA: gt_veiculo TYPE TABLE OF fleet.
    DATA: gt_EQUI TYPE TABLE OF equi.
    data: gt_KAEP_COAC TYPE TABLE OF KAEP_COAC.
    data: gt_KAEP TYPE TABLE OF KAEP_COAC.
    DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
    CREATE OBJECT obj_zcl_util_sd.
    DATA: vl_gdatu TYPE gdatu_inv,
          e_status TYPE char0001,
          e_messa  TYPE char0064.

    data: wa_data_general       TYPE bapi_itob,
          wa_return             TYPE bapiret2.

    FREE: gt_dados_cupons, gt_abastecimentos, gt_KAEP[].
    SELECT *
    FROM zpmt0024
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_cupons
      WHERE  CONFIRMA_RATEIO  EQ ABAP_TRUE
        AND  RATEIO_REALIZADO EQ ABAP_FALSE.

    CHECK  gt_dados_cupons IS NOT INITIAL.

    gt_dados_cupons_aux  = gt_dados_cupons.
    sort gt_dados_cupons_aux by pedido.
    delete ADJACENT DUPLICATES FROM gt_dados_cupons_aux COMPARING pedido.

    "Atualizar ordem para coleta de custo.
*      Check placa veiculo.
        FREE: gt_veiculo, gt_equi.
        SELECT *
        FROM fleet
        INTO CORRESPONDING FIELDS OF TABLE gt_veiculo
          FOR ALL ENTRIES IN gt_dados_cupons
        WHERE license_num EQ gt_dados_cupons-placa.

        IF sy-subrc eq 0.
        SELECT *
        FROM equi INTO CORRESPONDING FIELDS OF TABLE gt_equi
        FOR ALL ENTRIES IN gt_veiculo
        WHERE objnr EQ gt_veiculo-objnr.
        endif.

        loop at gt_dados_cupons ASSIGNING FIELD-SYMBOL(<ws_dados>).
        READ TABLE gt_veiculo into data(ws_veiculo) with key license_num = <ws_dados>-placa.
        if sy-subrc eq 0.
        READ TABLE gt_equi into data(ws_equi) with key objnr = ws_veiculo-objnr.
        if sy-subrc eq 0.

      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
      EXPORTING
        equipment        = ws_equi-equnr
      IMPORTING
        data_general_exp = wa_data_general
        return           = wa_return.

        if wa_data_general-standorder is not INITIAL.
          <ws_dados>-ORDEM_COMB = wa_data_general-standorder.
        endif.

        if wa_data_general-settlorder is not INITIAL.
           <ws_dados>-ORDEM_LUB = wa_data_general-settlorder.
        endif.
          endif.
        endif.
        clear: <ws_dados>, ws_veiculo, ws_equi, wa_data_general, wa_return.
        ENDLOOP.


*    SELECT *
*    FROM zpmt0026
*    INTO CORRESPONDING FIELDS OF TABLE gt_abastecimentos
*      FOR ALL ENTRIES IN gt_dados_cupons
*     WHERE fatura EQ gt_dados_cupons-fatura
*      AND cnpj EQ gt_dados_cupons-cnpj
*      and CUPOM_FISC eq gt_dados_cupons-CUPOM_FISC.

    CHECK gt_abastecimentos IS NOT INITIAL.
    SORT gt_abastecimentos ASCENDING BY qtde.
    DELETE gt_abastecimentos WHERE qtde EQ 0.
    SORT gt_abastecimentos ASCENDING BY cod_material.

*     SELECT *
*    FROM mseg
*    INTO TABLE @DATA(t_fatura)
*      FOR ALL ENTRIES IN @gt_dados_cupons
*      WHERE ebeln EQ @gt_dados_cupons-pedido
*        AND mblnr EQ @gt_dados_cupons-mblnr
*        AND smbln EQ @abap_false.

    FREE: t_ekpo.
    SELECT * FROM ekpo INTO TABLE t_ekpo
    FOR ALL ENTRIES IN gt_dados_cupons
    WHERE ebeln EQ gt_dados_cupons-pedido.
    SORT t_ekpo BY matnr.

    FREE: t_ekko.
    SELECT * FROM ekko INTO TABLE t_ekko
    FOR ALL ENTRIES IN t_ekpo
    WHERE ebeln EQ t_ekpo-ebeln.

*    SELECT *
*    FROM zpmt0034
*    INTO TABLE @DATA(t_zpmt0034)
*      FOR ALL ENTRIES IN @t_ekpo
*      WHERE matnr EQ @t_ekpo-matnr.
*    SORT t_zpmt0034 ASCENDING BY matnr.
*    DELETE ADJACENT DUPLICATES FROM t_zpmt0034 COMPARING matnr.
*    SORT gt_abastecimentos BY pedido cod_material.
*
*****Calculnado porcentagem referente a quantidade total do pedido por material.
*    LOOP AT gt_abastecimentos ASSIGNING FIELD-SYMBOL(<lw_abast>).
*      READ TABLE t_zpmt0034 INTO DATA(w_zpmt0034) WITH KEY cod_material = <lw_abast>-cod_material.
*      IF sy-subrc EQ 0.
*        <lw_abast>-cod_material = w_zpmt0034-matnr.
*      ENDIF.
*
*      <lw_abast>-pedido = |{ <lw_abast>-pedido ALPHA = IN }|.
*      <lw_abast>-cod_material = |{ <lw_abast>-cod_material ALPHA = IN }|.
*    ENDLOOP.


    "Coletar custo dos pedidos realizado para ordens dos pedidos.
     loop at GT_DADOS_CUPONS_AUX into data(ws_dados_aux).
       READ TABLE t_ekpo into data(ws_ekpo) with key ebeln = WS_DADOS_AUX-pedido.
       IF sy-subrc eq 0.
         READ TABLE t_ekko into data(ws_ekko) with key ebeln = WS_EKPO-ebeln.
         IF sy-subrc eq 0.
         READ TABLE t_cobl into data(ws_cobl) with key ebeln = WS_EKPO-ebeln.
         IF sy-subrc eq 0.


       free: gt_KAEP_COAC.
       CALL FUNCTION 'ZPM_GET_CUST_PEDIDO'
         EXPORTING
           I_AUFNR           = ws_cobl-aufnr
           I_BUDAT_INI       = ws_ekko-AEDAT
           I_BUDAT_FIM       = '99991231'
           I_BUKRS           = ws_ekko-bukrs
           I_WERKS           = ws_ekpo-werks
           I_EBELN           = ws_ekko-ebeln
         TABLES
           E_KAEP_COAC       = gt_KAEP_COAC.
       if gt_KAEP_COAC[] is not INITIAL.
         append LINES OF gt_KAEP_COAC to gt_KAEP[].
       endif.

        ENDIF.
        ENDIF.
       ENDIF.
       clear: ws_ekpo, ws_dados_aux, ws_ekko, ws_cobl.
     ENDLOOP.

***Verificar a sequencia da chave na tabela ZIB_CONTABIL.
    SELECT MAX( obj_key )
    FROM zib_contabil
    INTO z_obj_key
      WHERE obj_key LIKE 'RAT%'.

    CLEAR: z_ano_chav, z_ano_atual, w_obj_key, z_vlr_item, z_vlr_abast, z_tot_item.
    IF z_obj_key IS NOT INITIAL.
      z_ano_chav  = z_obj_key+16(4).
      z_ano_atual = sy-datum(04).
      w_obj_key   = z_obj_key+3(13).
      z_cod_chav  = z_obj_key(3).
    ELSE.
      z_ano_chav  = sy-datum(04).
      z_ano_atual = sy-datum(04).
      w_obj_key   = '0000000000000'.
      z_cod_chav = 'RAT'.
    ENDIF.

    DATA: obj_key TYPE zib_contabil-obj_key.
    SORT gt_abastecimentos ASCENDING BY pedido cod_material.

    SORT gt_KAEP BY EBELN REFBN.
    SORT gt_dados_cupons BY PEDIDO MBLNR.
    LOOP AT gt_dados_cupons ASSIGNING FIELD-SYMBOL(<_abast>).
    loop at gt_KAEP ASSIGNING FIELD-SYMBOL(<ws_KAEP>) WHERE REFBN = <_abast>-MBLNR AND EBELN EQ <_abast>-PEDIDO.

     IF <_abast>-ORDEM_COMB IS INITIAL.
       CONTINUE.
     ENDIF.
*//==============================================================================================================
******  1º item da chave.
      IF z_ano_chav EQ z_ano_atual.
        ADD 1 TO w_obj_key.
        w_obj_key                = |{ w_obj_key ALPHA = IN }|.
        gw_zib_contabil-obj_key  = z_cod_chav && w_obj_key && z_ano_chav.
        obj_key                  = z_cod_chav && w_obj_key && z_ano_chav.
      ELSE.
        ADD 1 TO w_obj_key.
        w_obj_key                = |{ w_obj_key ALPHA = IN }|.
        gw_zib_contabil-obj_key  = z_cod_chav && w_obj_key && z_ano_atual.
        obj_key                  = z_cod_chav && w_obj_key && z_ano_atual.
      ENDIF.

      gw_zib_contabil-seqitem  = '1'.
      gw_zib_contabil-bschl    = '50'.

        gw_zib_contabil-bukrs         = <ws_KAEP>-bukrs.

        "========================================================Inicio ajuste BUG 70164 - Anderson Oenning / 17/12/2021
        "Verificar se o periodo esta fechado.
        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
          EXPORTING
            i_bukrs  = <ws_KAEP>-bukrs
            i_data   = <ws_KAEP>-BUDAT
*           I_DEP_RESP = VG_DEPTO
          IMPORTING
            e_status = e_status
            e_messa  = e_messa
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.
        IF sy-subrc <> 0.

        ENDIF.
        DATA(s_data) = |{ sy-datum+06(02) }.{ sy-datum+04(02) }.{ sy-datum(04) }|.
        IF  e_status = 'E'. "Se o periodo estiver fechado, lançamento ira ficar com a data do dia atual.
          gw_zib_contabil-bldat = s_data.
          gw_zib_contabil-budat = s_data.
          gw_zib_contabil-gjahr = sy-datum(4).
          gw_zib_contabil-monat = sy-datum+4(2).
        ELSE.
          gw_zib_contabil-bldat         = |{ <ws_KAEP>-budat+06(02) }.{ <ws_KAEP>-budat+04(02) }.{ <ws_KAEP>-budat(04) }|.
          gw_zib_contabil-budat         = |{ <ws_KAEP>-budat+06(02) }.{ <ws_KAEP>-budat+04(02) }.{ <ws_KAEP>-budat(04) }|.
          gw_zib_contabil-gjahr         = <ws_KAEP>-budat(4).
          gw_zib_contabil-monat         = <ws_KAEP>-budat+4(2).
        ENDIF.
        "========================================================Fim ajuste.

        gw_zib_contabil-blart         = 'LR'.
        gw_zib_contabil-xblnr         = <ws_KAEP>-EBELN.
        gw_zib_contabil-hkont         = <ws_KAEP>-KSTAR.
        gw_zib_contabil-wrbtr         = <ws_KAEP>-AUFNR.
        gw_zib_contabil-waers         = 'BRL'.
        gw_zib_contabil-sgtxt         = 'Rateio pedido ' && <ws_KAEP>-EBELN && '->' && <ws_KAEP>-MATNR.
        gw_zib_contabil-bupla         = <ws_KAEP>-werks.
        gw_zib_contabil-aufnr         = <_abast>-ORDEM_COMB.
        gw_zib_contabil-waers_i       = 'BRL'.
*        gw_zib_contabil-dmbtr         = ( w_ekpo-netpr / w_ekpo-peinh ) * <_abast>-qtde. "( <_ABAST>-DMBTR * <_ABAST>-PERC ) / 100.
*        gw_zib_contabil-waers_f       = 'USD'.
*        gw_zib_contabil-dmbe2         = ( zdolar * <_abast>-qtde )."( <_ABAST>-DMBE2 * <_ABAST>-PERC ) / 100.
        gw_zib_contabil-rg_atualizado = 'N'.
        gw_zib_contabil-vornr = ' '.
        APPEND gw_zib_contabil TO gt_zib_contabil.
        CLEAR: gw_zib_contabil.


*//=====================================================================================================
******  2º item da chave.
      gw_zib_contabil-obj_key       = obj_key.
      gw_zib_contabil-seqitem       = '2'.
      gw_zib_contabil-bschl         = '40'.
*        GW_ZIB_CONTABIL-BSCHL         = '50'.

        "========================================================Inicio ajuste BUG 70164 - Anderson Oenning / 17/12/2021
        "Verificar se o periodo esta fechado.
        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
          EXPORTING
            i_bukrs  = <ws_KAEP>-bukrs
            i_data   = <ws_KAEP>-BUDAT
*           I_DEP_RESP = VG_DEPTO
          IMPORTING
            e_status = e_status
            e_messa  = e_messa
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.
        IF sy-subrc <> 0.

        ENDIF.

        s_data = |{ sy-datum+06(02) }.{ sy-datum+04(02) }.{ sy-datum(04) }|.
        IF  e_status = 'E'. "Se o periodo estiver fechado, lançamento ira ficar com a data do dia atual.
          gw_zib_contabil-bldat = s_data.
          gw_zib_contabil-budat = s_data.
          gw_zib_contabil-gjahr = sy-datum(4).
          gw_zib_contabil-monat = sy-datum+4(2).
        ELSE.
          gw_zib_contabil-bldat         = |{ <ws_KAEP>-budat+06(02) }.{ <ws_KAEP>-budat+04(02) }.{ <ws_KAEP>-budat(04) }|.
          gw_zib_contabil-budat         = |{ <ws_KAEP>-budat+06(02) }.{ <ws_KAEP>-budat+04(02) }.{ <ws_KAEP>-budat(04) }|.
          gw_zib_contabil-gjahr         = <ws_KAEP>-budat(4).
          gw_zib_contabil-monat         = <ws_KAEP>-budat+4(2).
        ENDIF.
        "========================================================Fim ajuste.

        gw_zib_contabil-blart         = 'LR'.
        gw_zib_contabil-xblnr         = <ws_KAEP>-EBELN.
        gw_zib_contabil-hkont         = <ws_KAEP>-KSTAR.
        gw_zib_contabil-wrbtr         = <ws_KAEP>-WTGBTR.
        gw_zib_contabil-waers         = 'BRL'.
        gw_zib_contabil-sgtxt         = 'Rateio pedido ' && <ws_KAEP>-EBELN && '->' && <ws_KAEP>-MATNR.
        gw_zib_contabil-bupla         = <ws_KAEP>-werks.
        gw_zib_contabil-aufnr         = <_abast>-ORDEM_COMB.
        gw_zib_contabil-waers_i       = 'BRL'.
*        gw_zib_contabil-dmbtr         = ( w_ekpo-netpr / w_ekpo-peinh ) * <_abast>-qtde. "( <_ABAST>-DMBTR * <_ABAST>-PERC ) / 100.
*        gw_zib_contabil-waers_f       = 'USD'.
*        gw_zib_contabil-dmbe2         = ( zdolar * <_abast>-qtde )."( <_ABAST>-DMBE2 * <_ABAST>-PERC ) / 100.
        gw_zib_contabil-rg_atualizado = 'N'.
        gw_zib_contabil-vornr = '0010'.


        gw_zpmt0063-obj_key =       gw_zib_contabil-obj_key.
        gw_zpmt0063-seqitem =       gw_zib_contabil-seqitem.
         gw_zpmt0063-bschl   =       gw_zib_contabil-bschl.
        gw_zpmt0063-gsber   =       gw_zib_contabil-gsber.
        gw_zpmt0063-MBLNR   =       <ws_KAEP>-REFBN.
        gw_zpmt0063-EBELN   =       <ws_KAEP>-EBELN.
        gw_zpmt0063-bukrs   =       gw_zib_contabil-bukrs.
        gw_zpmt0063-bldat   =       gw_zib_contabil-bldat.
        gw_zpmt0063-budat   =       gw_zib_contabil-budat.
        gw_zpmt0063-gjahr   =       gw_zib_contabil-gjahr.
        gw_zpmt0063-monat   =       gw_zib_contabil-monat.
        gw_zpmt0063-xblnr   =       gw_zib_contabil-xblnr.
        gw_zpmt0063-hkont   =       gw_zib_contabil-hkont.
        gw_zpmt0063-wrbtr   =       gw_zib_contabil-wrbtr.
        gw_zpmt0063-sgtxt   =       gw_zib_contabil-sgtxt.
        gw_zpmt0063-aufnr   =       gw_zib_contabil-aufnr.
        gw_zpmt0063-waers_i =       gw_zib_contabil-waers_i.
        gw_zpmt0063-dmbtr   =       gw_zib_contabil-dmbtr.
        gw_zpmt0063-waers_f =       gw_zib_contabil-waers_f.
        gw_zpmt0063-vornr         = gw_zib_contabil-vornr.
        gw_zpmt0063-cod_material  = <ws_KAEP>-MATNR.
        gw_zpmt0063-desc_material = <ws_KAEP>-CEL_KTXT.
        APPEND gw_zpmt0063 TO gt_zpmt0063.

        APPEND gw_zib_contabil TO gt_zib_contabil.
        CLEAR: gw_zib_contabil, gw_zpmt0063. "zdolar, vl_gdatu

    ENDLOOP.

      <_abast>-RATEIO_REALIZADO = ABAP_TRUE.

    ENDLOOP.

    DELETE gt_zib_contabil WHERE obj_key IS INITIAL.
    DELETE gt_zpmt0063     WHERE obj_key IS INITIAL. "PBI - 70620  - CBRAND
    MODIFY zib_contabil FROM TABLE gt_zib_contabil.
    MODIFY zpmt0063     FROM TABLE gt_zpmt0063. ""PBI - 70620  - CBRAND
    COMMIT WORK.
    IF sy-subrc EQ 0.

*//=========================================================================
**Modificando a tabela ZPMT0030 indicando que o rateio foi realizado.
*      LOOP AT gt_zpmt0030 ASSIGNING FIELD-SYMBOL(<ls_zpmt0030>).
*        <ls_zpmt0030>-conf_rateio = abap_true.
*        <ls_zpmt0030>-dt_atual    = sy-datum .
*        <ls_zpmt0030>-hr_atual    = sy-uzeit .
*        <ls_zpmt0030>-usuario     = sy-uname .
*      ENDLOOP.
*
*      MODIFY zpmt0030 FROM TABLE gt_zpmt0030.
*      COMMIT WORK.
    ENDIF.
    FREE: gt_zib_contabil, gt_zpmt0030,  gt_abastecimentos, gt_zib_contabil. "t_fatura, gt_bkpf, t_bseg, w_bsak
    CLEAR: obj_key.

  ENDMETHOD.


  METHOD SET_SATUS_PEDIDO.

    DATA: GT_FATURA TYPE TABLE OF ZPMT0025.
    DATA: T_FATURA TYPE TABLE OF ZPMT0024.
    DATA: LT_FATURA TYPE TABLE OF ZPMT0032.


    IF I_STATUS IS NOT INITIAL.
      IF I_STATUS EQ '05'.

*Selecionado fatura.
        SELECT *
        FROM ZPMT0025
        INTO TABLE GT_FATURA
          WHERE FATURA EQ I_FATURA
            AND CNPJ   EQ I_CNPJ.

        SELECT *
        FROM ZPMT0024
        INTO TABLE T_FATURA
          FOR ALL ENTRIES IN GT_FATURA
          WHERE FATURA EQ GT_FATURA-FATURA
            AND CNPJ   EQ I_CNPJ.

        SELECT *
       FROM ZPMT0032
       INTO TABLE LT_FATURA
         FOR ALL ENTRIES IN GT_FATURA
         WHERE FATURA EQ GT_FATURA-FATURA
           AND CNPJ   EQ I_CNPJ.

        DATA(T_ZPMT0025) = GT_FATURA.
"*---> 28/06/2023 - Migração S4 - LO
        SORT T_ZPMT0025 by FATURA.
"*---> 28/06/2023 - Migração S4 - LO
        DELETE ADJACENT DUPLICATES FROM T_ZPMT0025 COMPARING FATURA.

        READ TABLE T_ZPMT0025 INTO DATA(W_ZPMT0025) INDEX 1.

        LOOP AT GT_FATURA ASSIGNING FIELD-SYMBOL(<_PEDIDO>).
          <_PEDIDO>-COD_STATUS = '3'.
          <_PEDIDO>-STATUS_PROC = 'Pedido aprovado'.

          LOOP AT T_FATURA ASSIGNING FIELD-SYMBOL(<LS_FATURA>) WHERE FATURA EQ <_PEDIDO>-FATURA.
            <LS_FATURA>-COD_STATUS = '3'.
            <LS_FATURA>-STATUS_PROC = 'Pedido aprovado'.
          ENDLOOP.

          LOOP AT LT_FATURA ASSIGNING FIELD-SYMBOL(<L_FATURA>) WHERE FATURA EQ <_PEDIDO>-FATURA.
            <L_FATURA>-COD_STATUS = '3'.
            <L_FATURA>-STATUS_PROC = 'Pedido aprovado'.
          ENDLOOP.
        ENDLOOP.

        MODIFY ZPMT0024 FROM TABLE T_FATURA .
        MODIFY ZPMT0025 FROM TABLE GT_FATURA .
        MODIFY ZPMT0032 FROM TABLE LT_FATURA .

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        "Caso fatura esteja aprovada, criar JOB para criar os apontamento de medição de odometro e combustivel.
        READ TABLE LT_FATURA INTO DATA(W_FATURA) INDEX 1.
        IF SY-SUBRC EQ 0.
          "Precessa informações.
*          CALL METHOD ZCL_WEBSERVIC_PROTHEUS=>CALL_REPORT
*            EXPORTING
*              I_SEQUEN = CONV #( |{ W_FATURA-PEDIDO ALPHA = OUT }| )
*              I_REPORT = 'ZPMR0061'.
*   "// Finaliza o processo em caso de dados OffLine
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
