class ZCL_IMPOSTOS definition
  public
  final
  create public .

public section.

  class-methods GET_IMPOSTOS_DOCUMENTO
    importing
      !I_DOCNUM type J_1BDOCNUM
      !I_ITMNUM type J_1BITMNUM .
  class-methods GET_DENTRO_FORA_ESTADO_DOCNUM
    importing
      !I_DOCNUM type J_1BDOCNUM
    exporting
      !E_INDCOPER type CHAR01
      !E_TEXTO_FISCAL type CHAR50
      !E_SHIPFROM type REGIO
      !E_SHIPTO type REGIO .
  class-methods GET_CH_MODELO
    returning
      value(R_CH_MODELO) type ZCH_MODELO
    raising
      ZCX_ERROR .
  class-methods GET_TAX_IMPOSTO
    importing
      !I_DADOS type ZSDE0183
      !I_TODOS type CHAR01 default ABAP_OFF
    returning
      value(R_IMPOSTO) type ZSDT0370_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IMPOSTOS IMPLEMENTATION.


  METHOD get_ch_modelo.

    DATA: l_seq TYPE char9.

    FREE: r_ch_modelo.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZSD_CHMOD'
        toyear      = sy-datum(4)
      IMPORTING
        number      = l_seq.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_error
        EXPORTING
          textid = VALUE #( msgid = zcx_error=>zcx_erro_geral-msgid
                            msgno = zcx_error=>zcx_erro_geral-msgno
                            attr1 = CONV #( 'Não foi possivel gerar Numeração Modelo!' ) )
          msgid  = zcx_error=>zcx_erro_geral-msgid
          msgno  = zcx_error=>zcx_erro_geral-msgno
          msgty  = 'E'
          msgv1  = CONV #( 'Não foi possivel gerar Numeração Modelo!' ).
    ENDIF.

    r_ch_modelo = 'CH' && sy-datum(4) && l_seq.

  ENDMETHOD.


  method GET_DENTRO_FORA_ESTADO_DOCNUM.

    DATA: V_LIFNR TYPE LFA1-LIFNR.

    CLEAR: E_INDCOPER, E_TEXTO_FISCAL, E_SHIPFROM, E_SHIPTO.

    CHECK I_DOCNUM IS NOT INITIAL.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(WL_DOC)
     WHERE DOCNUM EQ @I_DOCNUM.

    CHECK SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM T001W INTO @DATA(WL_T001W)
     WHERE WERKS EQ @WL_DOC-BRANCH.

    CHECK SY-SUBRC EQ 0.

    SELECT SINGLE *
      FROM J_1BAD INTO @DATA(WL_J_1BAD)
     WHERE PARVW = @WL_DOC-PARVW.

    CHECK SY-SUBRC EQ 0.

    CASE WL_J_1BAD-PARTYP.
      WHEN 'C'.

        SELECT SINGLE *
          FROM KNA1 INTO @DATA(WL_KNA1)
         WHERE KUNNR EQ @WL_DOC-PARID.

      WHEN 'V'.

        SELECT SINGLE *
          FROM LFA1 INTO @DATA(WL_LFA1)
         WHERE LIFNR EQ @WL_DOC-PARID.

      WHEN 'B'.

        V_LIFNR = WL_DOC-PARID.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT     = V_LIFNR
          IMPORTING
            OUTPUT    = V_LIFNR.

        SELECT SINGLE *
          FROM LFA1 INTO WL_LFA1
         WHERE LIFNR EQ V_LIFNR+6(4).

    ENDCASE.

    CHECK SY-SUBRC EQ 0.

    CASE WL_J_1BAD-PARTYP.

      WHEN 'C'.

        IF WL_KNA1-REGIO EQ WL_T001W-REGIO.
          E_INDCOPER     = 'D'.
          E_TEXTO_FISCAL = 'Dentro do Estado'.
        ELSE.
          E_INDCOPER = 'F'.
          E_TEXTO_FISCAL = 'Fora do Estado'.
        ENDIF.

        IF WL_DOC-DIRECT EQ '1'.
          MOVE: WL_KNA1-REGIO TO E_SHIPFROM.
        ELSE.
          MOVE: WL_KNA1-REGIO TO E_SHIPTO.
        ENDIF.

      WHEN 'V' OR 'B'.

        IF WL_LFA1-REGIO EQ WL_T001W-REGIO.
          E_INDCOPER = 'D'.
          E_TEXTO_FISCAL = 'Dentro do Estado'.
        ELSE.
          E_INDCOPER = 'F'.
          E_TEXTO_FISCAL = 'Fora do Estado'.
        ENDIF.

        IF WL_DOC-DIRECT EQ '1'.
          MOVE: WL_LFA1-REGIO TO E_SHIPFROM.
        ELSE.
          MOVE: WL_LFA1-REGIO TO E_SHIPTO.
        ENDIF.

    ENDCASE.

    IF WL_DOC-DIRECT EQ '1'.
      MOVE: WL_T001W-REGIO TO E_SHIPTO.
    ELSE.
      MOVE: WL_T001W-REGIO TO E_SHIPFROM.
    ENDIF.

  endmethod.


  method GET_IMPOSTOS_DOCUMENTO.


  endmethod.


  METHOD get_tax_imposto.

*---------------------------
*-- Types
*---------------------------
    TYPES: BEGIN OF ty_tabela.
             INCLUDE TYPE zsdt0370.
    TYPES:   peso TYPE numc3.
    TYPES: END OF ty_tabela.

*---------------------------
*-- Var Locais
*---------------------------
    DATA: lr_range_auart      TYPE RANGE OF auart,
          lw_range_auart      LIKE LINE OF lr_range_auart,
          lr_range_vkaus      TYPE RANGE OF abrvw,
          lw_range_vkaus      LIKE LINE OF lr_range_vkaus,
          lr_range_brsch      TYPE RANGE OF brsch,
          lw_range_brsch      LIKE LINE OF lr_range_brsch,
          lr_range_uf_centro  TYPE RANGE OF regio,
          lw_range_uf_centro  LIKE LINE OF lr_range_uf_centro,
          lr_range_uf_cliente TYPE RANGE OF regio,
          lw_range_uf_cliente LIKE LINE OF lr_range_uf_cliente,
          lr_range_cityc      TYPE RANGE OF cityc,
          lw_range_cityc      LIKE LINE OF lr_range_cityc,
          lr_range_mwsk1      TYPE RANGE OF mwskz,
          lw_range_mwsk1      LIKE LINE OF lr_range_mwsk1,
          lr_range_ownpr      TYPE RANGE OF j_1bownpro,
          lw_range_ownpr      LIKE LINE OF lr_range_ownpr,
          lr_range_shtyp      TYPE RANGE OF shtyp,
          lw_range_shtyp      LIKE LINE OF lr_range_shtyp,
          lr_range_tdlnr      TYPE RANGE OF tdlnr,
          lw_range_tdlnr      LIKE LINE OF lr_range_tdlnr,
          lr_range_bukrs_toma TYPE RANGE OF bukrs,
          lw_range_bukrs_toma LIKE LINE OF lr_range_bukrs_toma,
          lr_range_bukrs_emit TYPE RANGE OF bukrs,
          lw_range_bukrs_emit LIKE LINE OF lr_range_bukrs_emit,
          lr_range_mtorg      TYPE RANGE OF j_1bmatorg,
          lw_range_mtorg      LIKE LINE OF lr_range_mtorg,
          lr_range_matnr      TYPE RANGE OF matnr,
          lw_range_matnr      LIKE LINE OF lr_range_matnr,
          lr_range_matkl      TYPE RANGE OF matkl,
          lw_range_matkl      LIKE LINE OF lr_range_matkl,
          lr_range_extwg      TYPE RANGE OF extwg,
          lw_range_extwg      LIKE LINE OF lr_range_extwg,
          lr_range_steuc      TYPE RANGE OF steuc,
          lw_range_steuc      LIKE LINE OF lr_range_steuc,
          lv_ref_struc        TYPE REF TO cl_abap_structdescr,
          lv_peso             TYPE numc3,
          lv_saiu             TYPE char01,
          lv_tabix            TYPE sy-tabix,
          t_tabela            TYPE TABLE OF ty_tabela,
          w_tabela            TYPE ty_tabela,
          t_fields            TYPE abap_compdescr_tab,
          lv_dados            TYPE zsde0183,
          lv_mtorg            TYPE mbew-mtorg,
          lv_steuc            TYPE marc-steuc,
          lv_matkl            TYPE mara-matkl,
          lv_extwg            TYPE mara-extwg,
          lv_ktokd            TYPE kna1-ktokd,
          lv_bukrs            TYPE t001-bukrs.

*---------------------------
*-- Field symbols
*---------------------------
    FIELD-SYMBOLS: <f_field1> TYPE any,
                   <f_value1> TYPE any,
                   <f_field2> TYPE any,
                   <f_value2> TYPE any,
                   <f_field3> TYPE any,
                   <f_value3> TYPE any,
                   <f_field4> TYPE any,
                   <f_field5> TYPE any,
                   <f_field6> TYPE any,
                   <f_field7> TYPE any,
                   <f_value8> TYPE STANDARD TABLE,
                   <f_field8> TYPE STANDARD TABLE.

    FREE: r_imposto.

*---------------------------
*-- campos da estrutura
*---------------------------
    lv_dados      = i_dados.
    lv_ref_struc ?= cl_abap_typedescr=>describe_by_name( 'ZSDT0370' ).
    t_fields[]    = lv_ref_struc->components[].

    DELETE t_fields WHERE name = 'MANDT'      OR name = 'CH_MODELO'  OR name = 'J_1BTXSDC'
                       OR name = 'J_1BTAXLW1' OR name = 'J_1BTAXLW2' OR name = 'J_1BTAXLW3'
                       OR name = 'J_1BTAXLW4' OR name = 'J_1BTAXLW5'.

*---------------------------
*-- Preenche algumas caractaristicas
*---------------------------
    IF lv_dados-bukrs_toma IS INITIAL AND lv_dados-kunnr <> abap_on.
      SELECT SINGLE ktokd
        INTO        lv_ktokd
        FROM kna1
       WHERE kunnr = lv_dados-kunnr-valor.

      IF sy-subrc = 0 AND lv_ktokd = 'ZCIC'.
        SELECT SINGLE bukrs
          INTO     lv_bukrs
          FROM j_1bbranch
         WHERE branch = lv_dados-kunnr-valor+6(4).

        IF sy-subrc = 0.
          lv_dados-bukrs_toma = COND #( WHEN lv_bukrs IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_bukrs ) ELSE abap_off ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_dados-mtorg IS INITIAL.
      SELECT SINGLE mtorg
        INTO     lv_mtorg
        FROM mbew
       WHERE matnr = lv_dados-matnr-valor
         AND bwkey = lv_dados-werks-valor.

      lv_dados-mtorg = COND #( WHEN lv_mtorg IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_mtorg ) ELSE abap_off ).
    ENDIF.

    IF lv_dados-steuc IS INITIAL.
      SELECT SINGLE steuc
        INTO     lv_steuc
        FROM marc
       WHERE matnr = lv_dados-matnr-valor
         AND werks = lv_dados-werks-valor.

      lv_dados-steuc = COND #( WHEN lv_steuc IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_steuc ) ELSE abap_off ).
    ENDIF.

    SELECT SINGLE matkl     extwg
      FROM mara
      INTO    (lv_matkl, lv_extwg)
     WHERE matnr = lv_dados-matnr-valor.

    IF lv_dados-matkl IS INITIAL.
      lv_dados-matkl = COND #( WHEN lv_matkl IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_matkl ) ELSE abap_off ).
    ENDIF.

    IF lv_dados-extwg IS INITIAL.
      lv_dados-extwg = COND #( WHEN lv_extwg IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_extwg ) ELSE abap_off ).
    ENDIF.

*---------------------------
*-- carrega os ranges para pesquisa
*---------------------------
    LOOP AT t_fields INTO DATA(w_fields).
      DATA(lv_field1) = 'LV_DADOS-' && w_fields-name.
      DATA(lv_field2) = 'LV_DADOS-' && w_fields-name && '-REGRA'.
      DATA(lv_field3) = 'LV_DADOS-' && w_fields-name && '-VALOR'.
      DATA(lv_field4) = 'LW_RANGE_' && w_fields-name && '-SIGN'.
      DATA(lv_field5) = 'LW_RANGE_' && w_fields-name && '-OPTION'.
      DATA(lv_field6) = 'LW_RANGE_' && w_fields-name && '-LOW'.
      DATA(lv_field7) = 'LW_RANGE_' && w_fields-name.
      DATA(lv_field8) = 'LR_RANGE_' && w_fields-name.

      ASSIGN (lv_field1)  TO <f_value1>.
      CHECK sy-subrc = 0 AND <f_value1> IS NOT INITIAL.

      ASSIGN (lv_field6)  TO <f_field6>.
      ASSIGN (lv_field7)  TO <f_field7>.
      ASSIGN (lv_field8)  TO <f_field8>.
      ASSIGN (lv_field4)  TO <f_field4>.
      ASSIGN (lv_field5)  TO <f_field5>.
      ASSIGN (lv_field6)  TO <f_field6>.
      ASSIGN (lv_field2)  TO <f_value2>.
      ASSIGN (lv_field3)  TO <f_value3>.

      <f_field4>           = 'I'.
      <f_field5>           = COND #( WHEN <f_value2> IS INITIAL THEN 'EQ' ELSE <f_value2> ).
      <f_field6>           = <f_value3>.

      APPEND <f_field7>   TO <f_field8>.
    ENDLOOP.

    CHECK lr_range_auart[] IS NOT INITIAL AND
          lr_range_vkaus[] IS NOT INITIAL AND
          lr_range_mwsk1[] IS NOT INITIAL.

*---------------------------
*-- Selecao tabela
*---------------------------
    SELECT *
      FROM zsdt0370
      INTO TABLE @DATA(t_zsdt0370)
     WHERE auart      IN @lr_range_auart
       AND vkaus      IN @lr_range_vkaus
       AND mwsk1      IN @lr_range_mwsk1
       AND uf_centro  IN @lr_range_uf_centro
       AND uf_cliente IN @lr_range_uf_cliente.

    CHECK sy-subrc = 0.

*---------------------------
*-- Priorizacao de pesquisa
*---------------------------
    LOOP AT t_zsdt0370       INTO DATA(w_zsdt0370).
      lv_tabix = sy-tabix.
      lv_peso  = 0.

      LOOP AT t_fields       INTO w_fields.
        DATA(lv_field_tab)      = 'W_ZSDT0370-'  && w_fields-name.
        ASSIGN (lv_field_tab)  TO <f_value1>.
        IF sy-subrc = 0 AND <f_value1> IS NOT INITIAL.
          lv_peso = lv_peso + 1.
        ENDIF.
      ENDLOOP.

      MOVE-CORRESPONDING w_zsdt0370  TO w_tabela.
      MOVE lv_peso                   TO w_tabela-peso.
      APPEND w_tabela                TO t_tabela.
    ENDLOOP.

*---------------------------
*-- Pesquisa das taxas / impostos
*---------------------------
    SORT t_tabela              BY peso DESCENDING.

    LOOP AT t_tabela         INTO w_tabela.
      lv_saiu = abap_off.

      LOOP AT t_fields       INTO w_fields.
        DATA(lv_field_rang)     = 'LR_RANGE_'  && w_fields-name && '[]'.
        DATA(lv_field_tabe)     = 'W_TABELA-'  && w_fields-name.
        ASSIGN (lv_field_rang) TO <f_value8>.
        ASSIGN (lv_field_tabe) TO <f_value2>.

        IF ( <f_value2> IS NOT INITIAL ) AND ( ( <f_value8>[] IS NOT INITIAL ) AND ( <f_value2> NOT IN <f_value8>[] ) ).
          lv_saiu = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_saiu = abap_off.
        APPEND w_tabela        TO r_imposto.
      ENDIF.

      IF i_todos IS INITIAL   AND r_imposto[] IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
