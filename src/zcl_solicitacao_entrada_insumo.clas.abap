class ZCL_SOLICITACAO_ENTRADA_INSUMO definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF LC_INCOTERMS,
        FOB TYPE C LENGTH 3 VALUE 'FOB',
        CIF TYPE C LENGTH 3 VALUE 'CIF',
        CPT TYPE C LENGTH 3 VALUE 'CPT',
      END OF LC_INCOTERMS .
  constants:
    BEGIN OF LC_SET,
        SEND_SPART_CARGUEIRO TYPE C LENGTH 20 VALUE 'ENVIO_SPART_CARGUERO',
      END OF LC_SET .
  class-data:
    R_SPART TYPE RANGE OF SPART .

  class-methods GET_CONVERSAO_UM
    importing
      !I_MATNR type MATNR
      !I_MEINS_INP type BSTME
      !I_MEINS_OUT type BSTME
      !I_MENGE type BSTMG
    returning
      value(R_MENGE) type BSTMG
    raising
      ZCX_ERROR .
  class-methods GET_SOLICITACAO_QTDE
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
      !I_SEQ type ZDE_SEQ optional
      !I_TP_SALDO type ZDE_TP_SALDO optional
    returning
      value(R_MENGE_KG) type BSTMG
    raising
      ZCX_ERROR .
  class-methods BLOQUEIA_SOLICITACAO
    importing
      !NRO_SOLICITACAO type ZDE_NRO_SOL
    exporting
      !BLOQUEADO type CHAR1
      !MENSAGEM_ERRO type STRING .
  class-methods DESBLOQUEIA_SOLICITACAO
    importing
      !NRO_SOLICITACAO type ZDE_NRO_SOL
    exporting
      !DESBLOQUEADO type CHAR1 .
  class-methods GET_AGENTE_FRETE
    importing
      !I_WERKS type LIFNR
    returning
      value(I_AGENTE) type LIFNR .
  class-methods CHECK_TIPO_FRETE
    importing
      !I_WERKS type EWERK
    exporting
      !E_INCO1 type INCO1
      !E_AGENTE type LIFNR .
  class-methods CHECK_DISPARO_CARGUERO
    importing
      !I_MATKL type MATKL
    returning
      value(IS_OK) type ABAP_BOOL .
  class-methods GET_OBSERVACOES
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
    returning
      value(R_OBSERVACOES) type STRING .
  class-methods GET_OBSERVACOES_LOTE_CARGUERO
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
    returning
      value(R_OBSERVACOES) type STRING .
  class-methods INTEGRAR_CARGUERO
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GRAVAR
    importing
      !I_ZMMT0196_T type ZMMT0196_T
    returning
      value(R_MSG_ERROR) type STRING .
  class-methods GET_SOL_QTDE_CARGA_MANUAL
    importing
      !I_NRO_SOL type ZDE_NRO_SOL
    returning
      value(R_MENGE) type BSTMG
    raising
      ZCX_ERROR .
  class-methods SALDO_SOLICITACAO
    importing
      !I_NRO_SOLICITACAO type ZDE_NRO_SOL optional
      !I_SEQ type ZDE_SEQ optional
      !I_NRO_CG type ZNRO_CG optional
      !I_ITENS type CHAR1 optional
      !I_TP_SALDO type ZDE_TP_SALDO optional
      !I_RETORNO_FULL type CHAR01 optional
    exporting
      !E_SALDO type ZSDT_SALDO_SOLIC .
  class-methods GET_SPART
    importing
      !I_NRO_SOLICITACAO type ZDE_NRO_SOL
    returning
      value(R_SPART) type SPART .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SOLICITACAO_ENTRADA_INSUMO IMPLEMENTATION.


  METHOD bloqueia_solicitacao.

    CALL FUNCTION 'ENQUEUE_EZMMOB_NRO_SOL'
      EXPORTING
        mode_zmmt0196  = 'E'
        mandt          = sy-mandt
        nro_sol        = nro_solicitacao
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      bloqueado = abap_true.
    ELSE.
      mensagem_Erro = | { 'Solicitação' } { nro_solicitacao } { 'Atualmente bloqueada por' } { sy-msgv1 } |.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_DISPARO_CARGUERO.

    SELECT VALSIGN   AS SIGN,
           VALOPTION AS OPTION,
           VALFROM   AS LOW
      INTO TABLE @R_SPART
        FROM SETLEAF
      WHERE SETNAME EQ @LC_SET-SEND_SPART_CARGUEIRO.

    CHECK R_SPART IS NOT INITIAL.

*    SELECT COUNT(*)
*        FROM MARA
*      WHERE MATKL EQ I_MATKL
*        AND SPART IN R_SPART.
*
*    CHECK SY-SUBRC IS INITIAL.

    SELECT COUNT(*)
        FROM ZMMT0200
      WHERE MATKL EQ I_MATKL
        AND SPART IN R_SPART.

    CHECK SY-SUBRC IS INITIAL.

    IS_OK = ABAP_ON.

  ENDMETHOD.


  METHOD CHECK_TIPO_FRETE.

    DATA: LV_ANSWER.
    DATA: LT_FIELDS TYPE TABLE OF SVAL.
    DATA: RETURNCODE(1)   TYPE C,
          POPUP_TITLE(30) TYPE C.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION         = 'Tipo de Agente de Frete é Proprio?'
        TEXT_BUTTON_1         = 'Sim'
        TEXT_BUTTON_2         = 'Não'
        START_COLUMN          = 30
        START_ROW             = 8
        DISPLAY_CANCEL_BUTTON = ' '
      IMPORTING
        ANSWER                = LV_ANSWER.

    IF LV_ANSWER EQ 1.
      E_INCO1 = ZCL_SOLICITACAO_ENTRADA_INSUMO=>LC_INCOTERMS-CIF.
    ELSE.
      E_INCO1 = ZCL_SOLICITACAO_ENTRADA_INSUMO=>LC_INCOTERMS-CPT.
      RETURN.
    ENDIF.

    CALL METHOD GET_AGENTE_FRETE
      EXPORTING
        I_WERKS  = CONV #( I_WERKS )
      RECEIVING
        I_AGENTE = E_AGENTE.

    APPEND
    VALUE #(
              TABNAME    = 'LFA1'
              FIELDNAME  = 'LIFNR'
              VALUE      = E_AGENTE
              FIELD_OBL  = 'X'
            ) TO LT_FIELDS.

    CLEAR E_AGENTE.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
      EXPORTING
        POPUP_TITLE = 'Informe o Agente de Frete'
        PROGRAMNAME = 'ZMMR209'
        FORMNAME    = 'F_CHECK_AGENTE_FRETE'
      IMPORTING
        RETURNCODE  = RETURNCODE
      TABLES
        FIELDS      = LT_FIELDS.

    READ TABLE LT_FIELDS INTO DATA(LS_FIELDS) INDEX 1.
    CHECK LS_FIELDS-VALUE IS NOT INITIAL.

    E_AGENTE = LS_FIELDS-VALUE.

  ENDMETHOD.


  METHOD DESBLOQUEIA_SOLICITACAO.

    CALL FUNCTION 'DEQUEUE_EZMMOB_NRO_SOL'
      EXPORTING
        MODE_ZMMT0196 = 'E'
        MANDT         = SY-MANDT
        NRO_SOL       = NRO_SOLICITACAO.

  ENDMETHOD.


  METHOD GET_AGENTE_FRETE.

    CHECK I_WERKS IS NOT INITIAL.

    DATA(LV_FORN) = |{ I_WERKS ALPHA = IN }|.

    SELECT SINGLE REGIO
      FROM LFA1
       INTO @DATA(LV_REGIO)
    WHERE LIFNR EQ @LV_FORN.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE VKORG
      FROM T001W
       INTO @DATA(LV_VKORG)
    WHERE WERKS EQ @I_WERKS.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE TDLNR
    FROM ZLEST0207
      INTO I_AGENTE
    WHERE BUKRS EQ LV_VKORG
      AND REGIO EQ LV_REGIO.

  ENDMETHOD.


  METHOD GET_CONVERSAO_UM.

    FREE: R_MENGE.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        I_MATNR              = I_MATNR
        I_IN_ME              = I_MEINS_INP
        I_OUT_ME             = I_MEINS_OUT
        I_MENGE              = I_MENGE
      IMPORTING
        E_MENGE              = R_MENGE
      EXCEPTIONS
        ERROR_IN_APPLICATION = 1
        ERROR                = 2
        OTHERS               = 3.

    IF SY-SUBRC <> 0.
      RAISE EXCEPTION TYPE ZCX_ERROR
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
                            MSGNO = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
                            ATTR1 = CONV #( 'Impossivel efetuar a Conversao DE UM.' ) )
          MSGID  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGID
          MSGNO  = ZCX_ERROR=>ZCX_ERRO_GERAL-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( 'Impossivel efetuar a Conversao DE UM.' ).
    ENDIF.

  ENDMETHOD.


  method GET_OBSERVACOES.

     DATA: wl_name  TYPE thead-tdname,
          lt_lines TYPE TABLE OF tline.


    CLEAR: r_observacoes, lt_lines[].

    CHECK i_nro_sol IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196)
     WHERE nro_sol EQ @i_nro_sol.

    CHECK sy-subrc EQ 0.

    wl_name = |SOL_EMBARQUE_{ i_nro_sol }|.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'OBSE'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZTEXTO'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    LOOP AT lt_lines INTO DATA(lwa_line).

      IF r_observacoes IS INITIAL.
        r_observacoes = lwa_line-tdline.
      ELSE.
        r_observacoes = |{ r_observacoes } { lwa_line-tdline }|.
      ENDIF.

    ENDLOOP.


  endmethod.


  METHOD get_observacoes_lote_carguero.

    DATA: lva_data_entrega TYPE char50.

    DATA: wl_name_rot   TYPE thead-tdname,
          it_texto      TYPE STANDARD TABLE OF tline,
          lva_texto_rot TYPE string.


    CLEAR: r_observacoes.

    CHECK i_nro_sol IS NOT INITIAL.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196)
     WHERE nro_sol EQ @i_nro_sol.

    CHECK sy-subrc EQ 0.

    r_observacoes = zcl_solicitacao_entrada_insumo=>get_observacoes( i_nro_sol = i_nro_sol ).

    lva_data_entrega = lwa_zmmt0196-entrega_dt+6(2) && '/' &&  lwa_zmmt0196-entrega_dt+4(2) && '/' &&  lwa_zmmt0196-entrega_dt(4).

    IF r_observacoes IS INITIAL.
      r_observacoes = |Data de Inicio de Embarque: { lva_data_entrega } |.
    ELSE.
      r_observacoes = |{ r_observacoes }! Data de Inicio de Embarque: { lva_data_entrega } |.
    ENDIF.


    " Seleciona dados Roteiro
    IF lwa_zmmt0196-rota_pc IS NOT INITIAL.

      CLEAR: it_texto[], lva_texto_rot.

      wl_name_rot = lwa_zmmt0196-rota_pc.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = wl_name_rot
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = it_texto
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

      LOOP AT it_texto INTO DATA(wa_texto).
        IF lva_texto_rot IS INITIAL.
          lva_texto_rot = wa_texto-tdline.
        ELSE.
          CONCATENATE lva_texto_rot wa_texto-tdline INTO lva_texto_rot SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lva_texto_rot is NOT INITIAL.
        r_observacoes = |{ r_observacoes } - Roteiro Ponto de Coleta: { lva_texto_rot } |.
      ENDIF.

    ENDIF.

    r_observacoes = zcl_string=>tira_acentos( i_texto = zcl_string=>convert_to_utf8( i_texto = CONV #( r_observacoes ) ) ).
    replace ALL OCCURRENCES OF ';' in r_observacoes WITH ','.


  ENDMETHOD.


  METHOD get_solicitacao_qtde.

    TYPES: BEGIN OF ty_solic,
             nro_sol                   TYPE zde_nro_sol,
             seq                       TYPE zde_seq,
             ebeln                     TYPE ebeln,
             ebelp                     TYPE ebelp,
             solicitacao_qte           TYPE zmmt0196-solicitacao_qte,
             solicitacao_qte_carguero  TYPE zmmt0196-solicitacao_qte_carguero,
             solicitacao_qte_manual    TYPE zmmt0196-solicitacao_qte_manual,
             cancel                    TYPE zmmt0196-cancel.
    TYPES: END   OF ty_solic.

    TYPES: BEGIN OF ty_ekpo,
             ebeln TYPE ebeln,
             ebelp TYPE ebelp,
             matnr TYPE matnr,
             meins TYPE bstme,
             menge TYPE bstmg.
    TYPES: END   OF ty_ekpo.

    DATA: t_solic TYPE TABLE OF ty_solic,
          t_ekpo  TYPE TABLE OF ty_ekpo,
          l_quant TYPE ekpo-menge,
          l_mesg  TYPE string,
          r_seq   TYPE RANGE OF zmmt0196-seq.

    FREE: r_menge_kg, r_seq.

    IF i_seq IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'BT' low = i_seq high = i_seq ) TO r_seq.
    ENDIF.

*-----------------------
*-- solicitacao
*-----------------------

    SELECT nro_sol seq ebeln ebelp
           solicitacao_qte
           solicitacao_qte_carguero
           solicitacao_qte_manual
           cancel
      INTO TABLE t_solic
      FROM zmmt0196
     WHERE nro_sol = i_nro_sol
       AND seq    IN r_seq.

    DELETE t_solic WHERE cancel EQ abap_true.

    CHECK t_solic[] IS NOT INITIAL.

    LOOP AT t_solic ASSIGNING FIELD-SYMBOL(<fs_solic>).

      CASE i_tp_saldo.
        WHEN 'C'. "Carguero
          <fs_solic>-solicitacao_qte = <fs_solic>-solicitacao_qte_carguero.
        WHEN 'M'. "Manual
          <fs_solic>-solicitacao_qte = <fs_solic>-solicitacao_qte_manual.
        WHEN OTHERS.
          <fs_solic>-solicitacao_qte = <fs_solic>-solicitacao_qte.
      ENDCASE.

    ENDLOOP.

*-----------------------
*-- itens solicitacao
*-----------------------
    SELECT ebeln ebelp matnr meins menge
      INTO TABLE t_ekpo
      FROM ekpo
       FOR ALL ENTRIES IN t_solic
     WHERE ebeln = t_solic-ebeln
       AND ebelp = t_solic-ebelp.

    SORT t_ekpo BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM t_ekpo COMPARING ebeln ebelp.

*-----------------------
*-- obtem quantidade
*-----------------------
    LOOP AT t_solic INTO DATA(w_solic).
      READ TABLE t_ekpo INTO DATA(w_ekpo) WITH KEY ebeln = w_solic-ebeln
                                                   ebelp = w_solic-ebelp
                                          BINARY SEARCH.
      CHECK sy-subrc = 0.

      IF w_ekpo-meins <> 'KG'.
        TRY.
            l_quant = get_conversao_um( i_matnr     = w_ekpo-matnr
                                        i_meins_inp = w_ekpo-meins
                                        i_meins_out = 'KG'
                                        i_menge     = w_solic-solicitacao_qte ).
            r_menge_kg = r_menge_kg + l_quant.

          CATCH zcx_error INTO DATA(ex_error).
            l_mesg = ex_error->get_text( ).
            RAISE EXCEPTION TYPE zcx_error
              EXPORTING
                textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                                  msgno = zcx_error=>zcx_erro_geral-msgno
                                  attr1 = l_mesg )
                msgid  = zcx_error=>zcx_erro_geral-msgid
                msgno  = zcx_error=>zcx_erro_geral-msgno
                msgty  = 'E'
                msgv1  = CONV #( l_mesg ).
        ENDTRY.
      ELSE.
        r_menge_kg = r_menge_kg + w_solic-solicitacao_qte.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_sol_qtde_carga_manual.

*    CLEAR: r_menge.
*
*    SELECT *
*      FROM zmmt0201 AS cb INTO TABLE @DATA(lit_carga_sem_item)
*     WHERE nro_sol = @i_nro_sol
*       AND NOT EXISTS ( SELECT nro_cg FROM zmmt0202 AS it WHERE it~nro_cg EQ cb~nro_cg ).
*
*    DELETE lit_carga_sem_item WHERE viagem_id IS INITIAL OR cancel EQ abap_true.
*
*    SELECT *
*      FROM zmmt0202 AS it INTO TABLE @DATA(lit_item_carga)
*     WHERE nro_sol = @i_nro_sol
*       AND EXISTS ( SELECT nro_cg
*                      FROM zmmt0201 AS cb
*                     WHERE cb~nro_cg    EQ it~nro_cg
*                       AND cb~cancel    EQ @abap_false
*                       AND cb~viagem_id IS INITIAL ).
*
*
*    DELETE t_solic WHERE cancel EQ abap_true.
*
*    CHECK t_solic[] IS NOT INITIAL.
*
**-----------------------
**-- itens solicitacao
**-----------------------
*    SELECT ebeln ebelp matnr meins menge
*      INTO TABLE t_ekpo
*      FROM ekpo
*       FOR ALL ENTRIES IN t_solic
*     WHERE ebeln = t_solic-ebeln
*       AND ebelp = t_solic-ebelp.
*
*    SORT t_ekpo BY ebeln ebelp.
*    DELETE ADJACENT DUPLICATES FROM t_ekpo COMPARING ebeln ebelp.
*
**-----------------------
**-- obtem quantidade
**-----------------------
*    LOOP AT t_solic INTO DATA(w_solic).
*      READ TABLE t_ekpo INTO DATA(w_ekpo) WITH KEY ebeln = w_solic-ebeln
*                                                   ebelp = w_solic-ebelp
*                                          BINARY SEARCH.
*      CHECK sy-subrc = 0.
*
*      IF w_ekpo-meins <> 'KG'.
*        TRY.
*            l_quant = get_conversao_um( i_matnr     = w_ekpo-matnr
*                                        i_meins_inp = w_ekpo-meins
*                                        i_meins_out = 'KG'
*                                        i_menge     = w_solic-solicitacao_qte ).
*            r_menge = r_menge + l_quant.
*
*          CATCH zcx_error INTO DATA(ex_error).
*            l_mesg = ex_error->get_text( ).
*            RAISE EXCEPTION TYPE zcx_error
*              EXPORTING
*                textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
*                                  msgno = zcx_error=>zcx_erro_geral-msgno
*                                  attr1 = l_mesg )
*                msgid  = zcx_error=>zcx_erro_geral-msgid
*                msgno  = zcx_error=>zcx_erro_geral-msgno
*                msgty  = 'E'
*                msgv1  = CONV #( l_mesg ).
*        ENDTRY.
*      ELSE.
*        r_menge = r_menge + w_ekpo-menge.
*      ENDIF.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD get_spart.

    CLEAR: r_spart.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196)
     WHERE nro_sol EQ @i_nro_solicitacao.

    CHECK sy-subrc EQ 0 AND i_nro_solicitacao IS NOT INITIAL.

    SELECT SINGLE *
      FROM ekpo INTO @DATA(lwa_ekpo)
     WHERE ebeln EQ @lwa_zmmt0196-ebeln.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM zmmt0200 INTO @DATA(lwa_zmmt0200)
      WHERE matkl EQ @lwa_ekpo-matkl.

    CHECK sy-subrc EQ 0.

    r_spart = lwa_zmmt0200-spart.

  ENDMETHOD.


  METHOD gravar.

    DATA: lva_msg_error_carguero TYPE string.

    DATA: lva_nro_sol TYPE zmmt0196-nro_sol.

    DATA: lwa_zmmt0196_old TYPE zmmt0196.

    DATA(lit_zmmt0196_gravar) = i_zmmt0196_t[].

    LOOP AT lit_zmmt0196_gravar ASSIGNING FIELD-SYMBOL(<fs_zmmt0196_gravar>).

      SELECT SINGLE *
        FROM ekpo INTO @DATA(lwa_ekpo)
       WHERE ebeln EQ @<fs_zmmt0196_gravar>-ebeln
         AND ebelp EQ @<fs_zmmt0196_gravar>-ebelp.

      IF sy-subrc NE 0.
        r_msg_error = 'Não foi possivel localizar o pedido da solicitação!'.
        RETURN.
      ENDIF.

      <fs_zmmt0196_gravar>-matkl = lwa_ekpo-matkl.

      IF <fs_zmmt0196_gravar>-matkl IS INITIAL.
        r_msg_error = 'Não foi possivel determinar o grupo de mercadoria para o item da solicitação!'.
        RETURN.
      ENDIF.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      TRY.
        DATA(_qtd_conv) = get_conversao_um( i_matnr     = conv #( lwa_ekpo-matnr )
                                            i_meins_inp = conv #( lwa_ekpo-meins )
                                            i_meins_out = 'KG'
                                            i_menge     = conv #( <fs_zmmt0196_gravar>-solicitacao_qte ) ).
      CATCH zcx_error INTO DATA(ex_error).
        r_msg_error = ex_error->get_text( ).
        r_msg_error = |Erro Conversão Material { lwa_ekpo-matnr } para KG: Erro: { r_msg_error }|.
        RETURN.
      ENDTRY.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

      IF <fs_zmmt0196_gravar>-inco1 EQ zcl_solicitacao_entrada_insumo=>lc_incoterms-fob. "Integrar Carguero
        <fs_zmmt0196_gravar>-solicitacao_qte_carguero = <fs_zmmt0196_gravar>-solicitacao_qte - <fs_zmmt0196_gravar>-solicitacao_qte_manual.
      ELSE.
        CLEAR: <fs_zmmt0196_gravar>-solicitacao_qte_carguero.
        <fs_zmmt0196_gravar>-solicitacao_qte_manual = <fs_zmmt0196_gravar>-solicitacao_qte.
      ENDIF.

    ENDLOOP.

    LOOP AT lit_zmmt0196_gravar INTO DATA(lwa_zmmt0196_gravar).

      lva_nro_sol = lwa_zmmt0196_gravar-nro_sol.

      CLEAR: r_msg_error, lva_msg_error_carguero, lwa_zmmt0196_old.

      "Recupera Registro antes da alteração
      SELECT SINGLE *
        FROM zmmt0196 INTO lwa_zmmt0196_old
       WHERE nro_sol EQ lwa_zmmt0196_gravar-nro_sol
         AND seq     EQ lwa_zmmt0196_gravar-seq.

      IF sy-subrc NE 0.
        CLEAR: lwa_zmmt0196_old.
      ENDIF.

      "Gravar Dados Solicitação
      MODIFY zmmt0196 FROM lwa_zmmt0196_gravar.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Não foi possivel salvar a solicitação!' TYPE 'S'.
        RETURN.
      ENDIF.

      "Validar Saldo
      zcl_solicitacao_entrada_insumo=>saldo_solicitacao(
        EXPORTING
          i_nro_solicitacao = lwa_zmmt0196_gravar-nro_sol
          i_seq             = lwa_zmmt0196_gravar-seq
          i_retorno_full    = abap_true
          i_itens           = abap_true
        IMPORTING
          e_saldo           = DATA(lit_sol_saldo_item)
      ).

      READ TABLE lit_sol_saldo_item INTO DATA(lwa_sol_saldo_item) WITH KEY nro_solic = lwa_zmmt0196_gravar-nro_sol
                                                                           seq       = lwa_zmmt0196_gravar-seq.
      IF ( sy-subrc NE 0 ) OR ( lit_sol_saldo_item[] IS INITIAL ).
        ROLLBACK WORK.
        MESSAGE 'Não foi possivel localizar o saldo da solicitação!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF lwa_sol_saldo_item-saldo < 0.
        ROLLBACK WORK.
        MESSAGE |Saldo total da solicitação ficará negativo em { lwa_sol_saldo_item-saldo } ! Operação não permitida| TYPE 'S'.
        RETURN.
      ENDIF.

      IF lwa_sol_saldo_item-saldo_carguero < 0.
        ROLLBACK WORK.
        MESSAGE |Saldo Carguero da solicitação ficará negativo em { lwa_sol_saldo_item-saldo_carguero }! Operação não permitida| TYPE 'S'.
        RETURN.
      ENDIF.

      IF lwa_sol_saldo_item-saldo_manual < 0.
        ROLLBACK WORK.
        MESSAGE |Saldo Manual da solicitação ficará negativo em { lwa_sol_saldo_item-saldo_manual }! Operação não permitida| TYPE 'S'.
        RETURN.
      ENDIF.

      IF lwa_sol_saldo_item-saldo_carguero > 0.
        SELECT SINGLE *
          FROM zlest0181 INTO @DATA(lwa_zlest0181)
         WHERE nro_sol EQ @lwa_zmmt0196_gravar-nro_sol.

        IF sy-subrc EQ 0 AND lwa_zlest0181-ck_cancelado EQ abap_true.
          ROLLBACK WORK.
          MESSAGE |Não pode ser atribuido Saldo Solicitação para o Carguero pois o lote embarcador da solicitação já foi cancelado! Operação não permitida| TYPE 'S'.
          RETURN.
        ENDIF.
      ENDIF.

      "Check se Integra ao Carguero/Strada - Ini
      IF lwa_zmmt0196_gravar-inco1 EQ zcl_solicitacao_entrada_insumo=>lc_incoterms-fob.

        lva_msg_error_carguero = zcl_solicitacao_entrada_insumo=>integrar_carguero( i_nro_sol = lwa_zmmt0196_gravar-nro_sol ).

        IF lva_msg_error_carguero IS NOT INITIAL.
          r_msg_error = lva_msg_error_carguero.

          "Reversão da Alteração. (Não há como usar rollback, porque há execução de um commit dentro do requisição de APi dentro da chamada do metodo anterior zcl_solicitacao_entrada_insumo=>integrar_carguero )
          IF lwa_zmmt0196_old IS NOT INITIAL.
            MODIFY zmmt0196 FROM lwa_zmmt0196_old.
          ELSE.
            DELETE FROM zmmt0196
             WHERE nro_sol EQ lwa_zmmt0196_gravar-nro_sol
               AND seq     EQ lwa_zmmt0196_gravar-seq.
          ENDIF.

          COMMIT WORK.

          MESSAGE |Houve um erro integrar a solicitação ao Carguero/Strada! Msg: { lva_msg_error_carguero }| TYPE 'S'.
          RETURN.
        ENDIF.

      ENDIF.
      "Check se Integra ao Carguero/Strada - Fim

      IF r_msg_error IS NOT INITIAL.
        ROLLBACK WORK.
        MESSAGE |Houve um erro salvar a solicitação! Msg: { r_msg_error }| TYPE 'S'.
        RETURN.
      ELSE.
        COMMIT WORK.
        MESSAGE |Solicitação { lva_nro_sol } gravada com sucesso!| TYPE 'S'.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD integrar_carguero.

    DATA: lit_zmmt0196   TYPE TABLE OF zmmt0196,
          lwa_zlest0181  TYPE zlest0181,
          lc_solicitacao TYPE zde_cargueiro_sl.

    CLEAR: r_msg_error, lit_zmmt0196[], lc_solicitacao, lwa_zlest0181.

    SELECT *
      FROM zmmt0196 INTO TABLE lit_zmmt0196
      WHERE nro_sol EQ i_nro_sol.

    IF lit_zmmt0196[] IS INITIAL OR i_nro_sol IS INITIAL.
      r_msg_error  = |Não encontrado os itens da solicitação { i_nro_sol }|.
      RETURN.
    ENDIF.

    READ TABLE lit_zmmt0196 INTO DATA(lwa_solicitacao) INDEX 1.

    IF sy-subrc NE 0.
      r_msg_error  = |Não encontrado os itens da solicitação { i_nro_sol }|.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zlest0181 INTO lwa_zlest0181
     WHERE nro_sol EQ i_nro_sol.

    CHECK lwa_solicitacao-inco1 EQ zcl_solicitacao_entrada_insumo=>lc_incoterms-fob.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando solicitação ao Carguero...'.

    MOVE-CORRESPONDING lwa_solicitacao TO lc_solicitacao.

    LOOP AT lit_zmmt0196 INTO DATA(w_zmmt0196).
      APPEND w_zmmt0196 TO lc_solicitacao-item.
    ENDLOOP.

    IF lwa_zlest0181 IS INITIAL. "Se não criou lote Embarcardor no Carguero, só prosseguir se tiver quantidade liberada para o Carguero
      TRY.
          DATA(lv_qtde_carguero) = zcl_solicitacao_entrada_insumo=>get_solicitacao_qtde( i_nro_sol  = i_nro_sol
                                                                                         i_tp_saldo = 'C' ).
        CATCH zcx_error INTO DATA(ex_error).
          r_msg_error = ex_error->get_text( ).
          RETURN.
      ENDTRY.

      CHECK lv_qtde_carguero > 0.
    ENDIF.

    TRY .
        zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
          EXPORTING
            i_solicitacao   = lc_solicitacao
            i_sincronia     = zif_integracao=>at_tp_sincronia_sincrona
          IMPORTING
            e_id_lote_frete = DATA(lv_id_lote_frete) ).

      CATCH zcx_integracao INTO DATA(ex_integracao).
        r_msg_error = ex_integracao->get_text( ).
        REPLACE ALL OCCURRENCES OF REGEX '&' IN r_msg_error WITH ''.
        RETURN.
      CATCH zcx_error INTO ex_error.
        r_msg_error = ex_error->get_text( ).
        REPLACE ALL OCCURRENCES OF REGEX '&' IN r_msg_error WITH ''.
        RETURN.
    ENDTRY.



  ENDMETHOD.


  METHOD saldo_solicitacao.

    DATA: lv_total        TYPE bstmg,
          lv_sum          TYPE zmmt0202-qtd_vinc_carga,
          lv_tot_kg       TYPE zmmt0202-qtd_vinc_carga,
          lv_menge        TYPE menge,
          lwa_zmmt0201    TYPE zmmt0201,
          r_seq           TYPE RANGE OF zmmt0196-seq,
          lv_unidpeso     TYPE ekpo-meins,
          lit_saldo_itens TYPE zsdt_saldo_solic.

    CLEAR: e_saldo, lit_saldo_itens[], lwa_zmmt0201.

    CHECK i_nro_solicitacao IS NOT INITIAL.

    IF i_seq IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_seq ) TO r_seq.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------------------------*
*   Selecionar Solicitações
*-----------------------------------------------------------------------------------------------------------------------------------------*

    CASE i_tp_saldo.
      WHEN 'G'. "Carga

        SELECT SINGLE *
          FROM zmmt0196 INTO @DATA(lwa_solicitacao_carga)
          WHERE nro_sol EQ @i_nro_solicitacao.

        CHECK sy-subrc EQ 0.

        SELECT *
          FROM zmmt0196 INTO TABLE @DATA(lt_0196)
         WHERE werks       EQ @lwa_solicitacao_carga-werks
           AND matkl       EQ @lwa_solicitacao_carga-matkl
           AND parceiro_pc EQ @lwa_solicitacao_carga-parceiro_pc
           AND parceiro_le EQ @lwa_solicitacao_carga-parceiro_le.

        SELECT SINGLE *
          FROM zmmt0201 INTO lwa_zmmt0201
         WHERE nro_cg EQ i_nro_cg.

      WHEN OTHERS.

        SELECT *
          FROM zmmt0196 INTO TABLE @lt_0196
          WHERE nro_sol EQ @i_nro_solicitacao
            AND seq     IN @r_seq.

    ENDCASE.


    IF i_retorno_full EQ abap_false.
      DELETE lt_0196[] WHERE cancel = abap_true.
    ENDIF.

    CHECK lt_0196[] IS NOT INITIAL.

*-----------------------------------------------------------------------------------------------------------------------------------------*
*   Organizar Prioridade
*-----------------------------------------------------------------------------------------------------------------------------------------*

    IF ( i_tp_saldo = 'G' ) AND  "Carga
       ( i_nro_solicitacao IS NOT INITIAL ).

      "Priorizar a Solicitação Principal
      DATA(lt_0196_aux) = lt_0196[].

      DELETE lt_0196 WHERE nro_sol NE i_nro_solicitacao. "Deixar somente solicitação principal
      SORT lt_0196 BY prioridade_item ASCENDING.

      "Adicionar Demais Solicitações com Saldo.
      DELETE lt_0196_aux WHERE nro_sol EQ i_nro_solicitacao.
      SORT lt_0196_aux BY date_create prioridade_item ASCENDING.
      LOOP AT lt_0196_aux ASSIGNING FIELD-SYMBOL(<fs_0196_aux>).
        APPEND INITIAL LINE TO lt_0196 ASSIGNING FIELD-SYMBOL(<fs_0196_add>).
        MOVE-CORRESPONDING <fs_0196_aux> TO <fs_0196_add>.
      ENDLOOP.

    ELSE.
      SORT lt_0196 BY date_create prioridade_item ASCENDING.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------------------------*
*   Selecionar Tabelas Auxiliares Solicitação
*-----------------------------------------------------------------------------------------------------------------------------------------*

    SELECT ebeln, ebelp, matnr, meins
      FROM ekpo INTO TABLE @DATA(lt_ekpo)
       FOR ALL ENTRIES IN @lt_0196
     WHERE ebeln EQ @lt_0196-ebeln
       AND ebelp EQ @lt_0196-ebelp.

    SORT lt_ekpo[] BY ebeln ebelp.

    SELECT *
      FROM zmmt0202 AS a INTO TABLE @DATA(lt_0202)
      FOR ALL ENTRIES IN @lt_0196
      WHERE nro_sol EQ @lt_0196-nro_sol
       AND seq      EQ @lt_0196-seq
       AND EXISTS ( SELECT nro_cg
                      FROM zmmt0201 AS b
                     WHERE b~nro_cg = a~nro_cg
                       AND b~cancel = @abap_false ).

    DELETE lt_0202 WHERE cancel EQ abap_true.

    IF lt_0202[] IS NOT INITIAL.
      SELECT *
        FROM zmmt0201 AS a INTO TABLE @DATA(lt_0201)
        FOR ALL ENTRIES IN @lt_0202
        WHERE nro_cg = @lt_0202-nro_cg.

      DELETE lt_0201 WHERE cancel EQ abap_true.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------------------------*
*   Calcular Saldo das Solicitações
*-----------------------------------------------------------------------------------------------------------------------------------------*

    LOOP AT lt_0196 ASSIGNING FIELD-SYMBOL(<fs_0196>).
      LOOP AT lt_0202 INTO DATA(lwa_0202) WHERE nro_sol = <fs_0196>-nro_sol
                                            AND seq     = <fs_0196>-seq.

        READ TABLE lt_0201 INTO DATA(lwa_0201) WITH KEY nro_cg = lwa_0202-nro_cg.
        CHECK sy-subrc EQ 0.

        IF i_tp_saldo EQ 'G'. "Carga
          CHECK lwa_0201-nro_cg NE i_nro_cg. "Saldo não deve contabilizar consumos da mesma Carga.
        ENDIF.

        SUBTRACT lwa_0202-qtd_vinc_carga FROM <fs_0196>-solicitacao_qte.

        IF lwa_0202-tp_saldo_vinc EQ 'C'. "Carguero
          SUBTRACT lwa_0202-qtd_vinc_carga FROM <fs_0196>-solicitacao_qte_carguero.
        ELSE.
          SUBTRACT lwa_0202-qtd_vinc_carga FROM <fs_0196>-solicitacao_qte_manual.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    CASE i_tp_saldo.
      WHEN 'C'. "Carguero
        IF i_retorno_full EQ abap_false.
          DELETE lt_0196 WHERE solicitacao_qte_carguero <= 0.
        ENDIF.
      WHEN 'M'. "Manual
        IF i_retorno_full EQ abap_false.
          DELETE lt_0196 WHERE solicitacao_qte_manual <= 0.
        ENDIF.
      WHEN 'G'. "Carga.

        "Desmembrar saida de Saldo em: Saldo Carguero e Saldo Manual...
        DATA(lt_0196_desmembrar) = lt_0196[].
        CLEAR: lt_0196.

        LOOP AT lt_0196_desmembrar ASSIGNING FIELD-SYMBOL(<fs_0196_desmembrar>).

          IF <fs_0196_desmembrar>-solicitacao_qte_carguero > 0 AND <fs_0196_desmembrar>-nro_sol EQ lwa_solicitacao_carga-nro_sol.
            APPEND INITIAL LINE TO lt_0196 ASSIGNING <fs_0196>.
            MOVE-CORRESPONDING <fs_0196_desmembrar> TO <fs_0196>.
            CLEAR: <fs_0196>-solicitacao_qte_manual.
          ENDIF.

          IF <fs_0196_desmembrar>-solicitacao_qte_manual > 0.
            APPEND INITIAL LINE TO lt_0196 ASSIGNING <fs_0196>.
            MOVE-CORRESPONDING <fs_0196_desmembrar> TO <fs_0196>.
            CLEAR: <fs_0196>-solicitacao_qte_carguero.
          ENDIF.

        ENDLOOP.

        IF i_retorno_full EQ abap_false.
          DELETE lt_0196 WHERE solicitacao_qte_manual <= 0 AND solicitacao_qte_carguero <= 0.
        ENDIF.

      WHEN OTHERS.

        IF i_retorno_full EQ abap_false.
          DELETE lt_0196 WHERE solicitacao_qte <= 0.
        ENDIF.

    ENDCASE.

*-----------------------------------------------------------------------------------------------------------------------------------------*
*   Montar Saida de Saldo das Solicitações
*-----------------------------------------------------------------------------------------------------------------------------------------*

    LOOP AT lt_0196 ASSIGNING <fs_0196>.

      CLEAR: lv_tot_kg.

      APPEND INITIAL LINE TO lit_saldo_itens ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      <fs_saldo>-nro_solic       = <fs_0196>-nro_sol.
      <fs_saldo>-seq             = <fs_0196>-seq.
      <fs_saldo>-ebeln           = <fs_0196>-ebeln.
      <fs_saldo>-ebelp           = <fs_0196>-ebelp.
      <fs_saldo>-saldo_carguero  = <fs_0196>-solicitacao_qte_carguero.
      <fs_saldo>-saldo_manual    = <fs_0196>-solicitacao_qte_manual.

      CASE i_tp_saldo.
        WHEN 'C'. "Carguero
          <fs_saldo>-saldo     = <fs_0196>-solicitacao_qte_carguero.
          <fs_saldo>-tp_saldo  = 'C'. "Carguero
        WHEN 'M'. "Manual
          <fs_saldo>-saldo     = <fs_0196>-solicitacao_qte_manual.
          <fs_saldo>-tp_saldo  = 'M'. "Manual
        WHEN 'G'. "Carga

          IF <fs_0196>-solicitacao_qte_carguero > 0.
            <fs_saldo>-saldo     = <fs_0196>-solicitacao_qte_carguero.
            <fs_saldo>-tp_saldo  = 'C'. "Carguero
          ELSEIF <fs_0196>-solicitacao_qte_manual > 0.
            <fs_saldo>-saldo     = <fs_0196>-solicitacao_qte_manual.
            <fs_saldo>-tp_saldo  = 'M'. "Manual
          ENDIF.

        WHEN OTHERS.
          <fs_saldo>-saldo     = <fs_0196>-solicitacao_qte.
      ENDCASE.

      lv_total             = <fs_saldo>-saldo.

      READ TABLE lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>) WITH KEY ebeln = <fs_0196>-ebeln
                                                                    ebelp = <fs_0196>-ebelp BINARY SEARCH.
      CHECK sy-subrc = 0.

      <fs_saldo>-matnr = <fs_ekpo>-matnr.

      lv_unidpeso = <fs_ekpo>-meins.

      "Conversão KG
      IF lv_total IS NOT INITIAL.

        CALL METHOD zcl_solicitacao_entrada_insumo=>get_conversao_um
          EXPORTING
            i_matnr     = <fs_ekpo>-matnr
            i_meins_inp = lv_unidpeso
            i_meins_out = 'KG'
            i_menge     = lv_total
          RECEIVING
            r_menge     = lv_tot_kg.

        IF lv_tot_kg IS NOT INITIAL.
          <fs_saldo>-saldo_kg  = lv_tot_kg.
          <fs_saldo>-embalagem = lv_tot_kg / lv_total.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF lwa_zmmt0201-nro_cg IS NOT INITIAL AND lwa_zmmt0201-viagem_id IS INITIAL."Se for carga Manual(Não criada pelo Carguero), só exibir Saldos Manuais da solicitação
      DELETE lit_saldo_itens WHERE tp_saldo NE 'M'.
    ENDIF.

    CASE i_itens.
      WHEN abap_true.

        e_saldo = lit_saldo_itens.

      WHEN abap_false.

        APPEND INITIAL LINE TO e_saldo ASSIGNING <fs_saldo>.

        <fs_saldo>-nro_solic = i_nro_solicitacao.

        LOOP AT lit_saldo_itens ASSIGNING FIELD-SYMBOL(<fs_saldo_item>).
          ADD <fs_saldo_item>-saldo    TO <fs_saldo>-saldo.
          ADD <fs_saldo_item>-saldo_kg TO <fs_saldo>-saldo_kg.
        ENDLOOP.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
