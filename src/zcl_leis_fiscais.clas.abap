class ZCL_LEIS_FISCAIS definition
  public
  final
  create public .

public section.

  class-methods GET_IMPOSTOS
    importing
      !I_DADOS type ZSDE0185
      !I_TODOS type CHAR01 default ABAP_OFF
    returning
      value(R_IMPOSTO) type ZMMT0154_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_LEIS_FISCAIS IMPLEMENTATION.


  METHOD get_impostos.

*---------------------------
*-- Types
*---------------------------
    TYPES: BEGIN OF ty_tabela.
             INCLUDE TYPE zmmt0154.
    TYPES:   peso TYPE numc3.
    TYPES: END OF ty_tabela.

*---------------------------
*-- Var Locais
*---------------------------
    DATA: lr_range_bsart   TYPE RANGE OF bsart,
          lw_range_bsart   LIKE LINE OF lr_range_bsart,
          lr_range_uf_orig TYPE RANGE OF regio,
          lw_range_uf_orig LIKE LINE OF lr_range_uf_orig,
          lr_range_uf_dest TYPE RANGE OF regio,
          lw_range_uf_dest LIKE LINE OF lr_range_uf_dest,
          lr_range_ownpr   TYPE RANGE OF j_1bownpro,
          lw_range_ownpr   LIKE LINE OF lr_range_ownpr,
          lr_range_matnr   TYPE RANGE OF matnr,
          lw_range_matnr   LIKE LINE OF lr_range_matnr,
          lr_range_matkl   TYPE RANGE OF matkl,
          lw_range_matkl   LIKE LINE OF lr_range_matkl,
          lr_range_bukrs   TYPE RANGE OF bukrs,
          lw_range_bukrs   LIKE LINE OF lr_range_bukrs,
          lr_range_mtorg   TYPE RANGE OF j_1bmatorg,
          lw_range_mtorg   LIKE LINE OF lr_range_mtorg,
          lr_range_direcao TYPE RANGE OF j_1bdirect,
          lw_range_direcao LIKE LINE OF lr_range_direcao,
          lr_range_icms    TYPE RANGE OF zmmt_icms,
          lw_range_icms    LIKE LINE OF lr_range_icms,
          lv_ref_struc     TYPE REF TO cl_abap_structdescr,
          lv_peso          TYPE numc3,
          lv_saiu          TYPE char01,
          lv_tabix         TYPE sy-tabix,
          t_tabela         TYPE TABLE OF ty_tabela,
          w_tabela         TYPE ty_tabela,
          t_fields         TYPE abap_compdescr_tab,
          lv_dados         TYPE zsde0185,
          lv_werks_d       TYPE lfa1-lifnr,
          lv_bukrs         TYPE ekko-bukrs,
          lv_mtorg         TYPE mbew-mtorg,
          lv_ownpr         TYPE mbew-ownpr,
          lv_matnr         TYPE mara-matnr,
          lv_matkl         TYPE mara-matkl,
          lv_extwg         TYPE mara-extwg,
          lv_regio_orig    TYPE lfa1-regio,
          lv_regio_dest    TYPE lfa1-regio,
          lv_forn_orig     TYPE lfa1-lifnr,
          lv_forn_dest     TYPE lfa1-lifnr.

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
    lv_ref_struc ?= cl_abap_typedescr=>describe_by_name( 'ZMMT0154' ).
    t_fields[]    = lv_ref_struc->components[].

    DELETE t_fields WHERE name = 'MANDT'      OR name = 'MWSKZ'
                       OR name = 'J_1BTAXLW1' OR name = 'J_1BTAXLW2' OR name = 'J_1BTAXLW3'
                       OR name = 'J_1BTAXLW4' OR name = 'J_1BTAXLW5' OR name = 'CFOP'.

*---------------------------
*-- Preenche algumas caractaristicas
*---------------------------
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lv_dados-matnr-valor
      IMPORTING
        output = lv_matnr.

    IF lv_dados-bukrs   IS INITIAL AND lv_dados-ebeln IS NOT INITIAL.
      SELECT SINGLE bukrs, bsart, reswk, lifnr
        INTO @DATA(_ekko)
        FROM ekko
       WHERE ebeln = @lv_dados-ebeln-valor.

      lv_dados-bukrs = COND #( WHEN lv_bukrs IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_bukrs ) ELSE abap_off ).
    ENDIF.

    IF lv_dados-uf_orig IS INITIAL AND lv_dados-ebeln IS NOT INITIAL.
      SELECT SINGLE bukrs, bsart, reswk, lifnr
        INTO @_ekko
        FROM ekko
       WHERE ebeln = @lv_dados-ebeln-valor.

      IF sy-subrc = 0.
        IF _ekko-bsart = 'ZUB'  .
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = _ekko-reswk
            IMPORTING
              output = lv_forn_orig.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = _ekko-lifnr
            IMPORTING
              output = lv_forn_orig.
        ENDIF.

        SELECT SINGLE regio FROM lfa1 INTO lv_regio_orig WHERE lifnr = lv_forn_orig.

        lv_dados-uf_orig = COND #( WHEN lv_regio_orig IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_regio_orig ) ELSE abap_off ).
      ENDIF.
    ENDIF.

    IF lv_dados-uf_dest IS INITIAL AND lv_dados-ebeln IS NOT INITIAL.
      SELECT SINGLE  bsart, reswk, lifnr
        INTO @_ekko
        FROM ekko
       WHERE ebeln = @lv_dados-ebeln.

      IF sy-subrc = 0.
        IF 'ZUB_ZARM_ZARS' CS _ekko-bsart.
          SELECT SINGLE werks
            INTO lv_werks_d
            FROM ekpo
           WHERE ebeln = lv_dados-ebeln.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_werks_d
            IMPORTING
              output = lv_forn_dest.

          SELECT SINGLE regio FROM lfa1 INTO lv_regio_dest WHERE lifnr = lv_forn_dest.

          lv_dados-uf_dest = COND #( WHEN lv_regio_dest IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_regio_dest ) ELSE abap_off ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( lv_dados-mtorg IS  INITIAL  OR lv_dados-ownpr IS INITIAL ) AND
       ( lv_dados-matnr IS NOT INITIAL AND lv_dados-werks IS NOT INITIAL ).

      SELECT SINGLE mtorg     ownpr
        INTO    (lv_mtorg, lv_ownpr)
        FROM mbew
       WHERE matnr = lv_matnr
         AND bwkey = lv_dados-werks-valor.

      lv_dados-mtorg = COND #( WHEN lv_mtorg IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_mtorg ) ELSE abap_off ).
      lv_dados-ownpr = VALUE #( regra = 'EQ' valor = lv_ownpr ).
    ENDIF.

    IF lv_dados-matkl IS INITIAL AND lv_dados-matnr IS NOT INITIAL.
      SELECT SINGLE matkl
        INTO     lv_matkl
        FROM mara
       WHERE matnr = lv_matnr.

      lv_dados-matkl = COND #( WHEN lv_matkl IS NOT INITIAL THEN VALUE #( regra = 'EQ' valor = lv_matkl ) ELSE abap_off ).
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

    CHECK lr_range_bsart[] IS NOT INITIAL.

*---------------------------
*-- Selecao tabela
*---------------------------

    CASE lv_dados-direcao-valor.
      WHEN '1'. "Entrada

        SELECT *
          FROM zmmt0154 INTO TABLE @DATA(t_zmmt0154)
         WHERE bsart      IN @lr_range_bsart
           AND uf_orig    IN @lr_range_uf_orig
           AND uf_dest    IN @lr_range_uf_dest
           AND icms       IN @lr_range_icms.

      WHEN '2'. "Saida

        SELECT *
          FROM zmmt0154 INTO TABLE @t_zmmt0154
         WHERE bsart      IN @lr_range_bsart
           AND uf_orig    IN @lr_range_uf_orig
           AND uf_dest    IN @lr_range_uf_dest.

    ENDCASE.


    CHECK t_zmmt0154[] is NOT INITIAL.

*---------------------------
*-- Priorizacao de pesquisa
*---------------------------
    LOOP AT t_zmmt0154       INTO DATA(w_zmmt0154).
      lv_tabix = sy-tabix.
      lv_peso  = 0.

      LOOP AT t_fields       INTO w_fields.
        DATA(lv_field_tab)      = 'W_ZMMT0154-'  && w_fields-name.
        ASSIGN (lv_field_tab)  TO <f_value1>.
        IF sy-subrc = 0 AND <f_value1> IS NOT INITIAL.
          lv_peso = lv_peso + 1.
        ENDIF.
      ENDLOOP.

      MOVE-CORRESPONDING w_zmmt0154  TO w_tabela.
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
