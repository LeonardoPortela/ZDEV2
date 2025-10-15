FUNCTION z_mm_indea_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EBELN) TYPE  EBELN
*"     REFERENCE(I_EBELP) TYPE  EBELP
*"     REFERENCE(I_LINE_ID) TYPE  MB_LINE_ID OPTIONAL
*"     REFERENCE(I_MBLNR) TYPE  MBLNR OPTIONAL
*"     REFERENCE(I_MJAHR) TYPE  MJAHR OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_CHARG) TYPE  CHARG_D
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_LGORT) TYPE  LGORT_D OPTIONAL
*"     REFERENCE(I_MENGE) TYPE  MENGE_D
*"     REFERENCE(I_BTN) TYPE  CHAR1 DEFAULT 'X'
*"     REFERENCE(I_TCODE) TYPE  TCODE
*"     REFERENCE(I_RENAS) TYPE  ATWRT OPTIONAL
*"     REFERENCE(T_CARAC) TYPE  ZIB_NFE_DIST_LCA_T OPTIONAL
*"----------------------------------------------------------------------
  DATA: idd07v TYPE TABLE OF  dd07v WITH HEADER LINE.
  DATA  vdomvalue_l TYPE  dd07v-domvalue_l.
  DATA: tabix TYPE sy-tabix.

*-CS2025000249-17.04.2025-#173311-JT-inicio
  FREE: l_categoria.
  READ TABLE t_carac INTO DATA(_carac) WITH KEY atinn = '0000064797'.  "CATEGORIA SEMENTE
  IF sy-subrc = 0.
    l_categoria = _carac-atwrt.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZCATEGSEMEN'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    READ TABLE idd07v INTO DATA(w_idd07v) WITH KEY domvalue_l = l_categoria.
    IF sy-subrc = 0.
      l_descategoria = w_idd07v-ddtext.
    ENDIF.
  ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

  wa_zmmt0102-ebeln     = i_ebeln.
  wa_zmmt0102-ebelp     = i_ebelp.
  wa_zmmt0102-line_id   = i_line_id.
  wa_zmmt0102-mblnr     = i_mblnr.
  wa_zmmt0102-mjahr     = i_mjahr.
  wa_zmmt0102-matnr     = i_matnr.
  wa_zmmt0102-werks     = i_werks.
  wa_zmmt0102-charg     = i_charg.
  wa_zmmt0102-lgort     = i_lgort.
  wa_zmmt0102-menge     = i_menge.
  wa_zmmt0102-renas     = i_renas.

  p_btn   = i_btn.
  p_charg = i_charg.
  p_menge = i_menge.
  p_tcode = i_tcode.
  p_renas = i_renas.

  IF p_tcode = 'ZSDT0112'.
    p_tcode = 'ZMM0110'.
  ENDIF.

  IF p_tcode = 'MIGO'.

    SELECT SINGLE bsart
      INTO @DATA(vbsart)
      FROM ekko
      WHERE ebeln = @i_ebeln.

    CHECK vbsart = 'ZSEM'.

  ELSEIF p_tcode = 'ZMM0110' OR
         p_tcode = 'ZLES0200'. "*-CS2024000522-18.07.2024-JT-#147087

    SELECT SINGLE * INTO @DATA(wa_ekpo)
      FROM ekpo
     WHERE ebeln EQ @i_ebeln
       AND ebelp EQ @i_ebelp.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_t001w)
      FROM t001w
     WHERE werks EQ @wa_ekpo-werks.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO @DATA(wa_zmmt0129)
      FROM zmmt0129
     WHERE regio_destino EQ @wa_t001w-regio
       AND matkl EQ @wa_ekpo-matkl.

    CHECK sy-subrc IS INITIAL.

  ELSEIF p_tcode NE 'ZNFW0002'.
    EXIT.
  ENDIF.
  "
  IF i_btn = 'X'.
    IF p_tcode = 'MIGO'.
      SELECT  *
          FROM zmmt0102
          INTO CORRESPONDING FIELDS OF TABLE tg_lotes
          WHERE ebeln   = i_ebeln
          AND   ebelp   = i_ebelp
          AND   mblnr   = i_mblnr
          AND   mjahr   = i_mjahr
          AND   line_id = i_line_id
*        AND   CHARG = I_CHARG
          AND   tcode   = i_tcode.
    ELSE.
      SELECT  *
       FROM zmmt0102
       INTO CORRESPONDING FIELDS OF TABLE tg_lotes
       WHERE ebeln   = i_ebeln
       AND   ebelp   = i_ebelp
       AND   mblnr   = i_mblnr
       AND   mjahr   = i_mjahr
       AND   line_id = i_line_id
       AND   charg   = i_charg
       AND   tcode   = i_tcode.
    ENDIF.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = 'ZCATEGSEMEN'
        text           = 'X'
        langu          = sy-langu
      TABLES
        dd07v_tab      = idd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    LOOP AT tg_lotes.
      tabix = sy-tabix.
      vdomvalue_l = tg_lotes-categoria.
      READ TABLE idd07v WITH KEY domvalue_l = vdomvalue_l.
      tg_lotes-descategoria = idd07v-ddtext.
      MODIFY tg_lotes INDEX tabix TRANSPORTING descategoria.
    ENDLOOP.

    IF l_categoria IS NOT INITIAL.  "*-CS2025000249-17.04.2025-#173311-JT
      CALL SCREEN 100 ENDING AT 110 09 STARTING AT 18 3.
    ELSE.
      MESSAGE s024(sd) WITH 'Categoria Semente não foi informada!' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    SELECT  *
        FROM zmmt0102
        INTO CORRESPONDING FIELDS OF TABLE tg_lotes
        WHERE ebeln = i_ebeln
        AND   ebelp = i_ebelp
        AND   mblnr = ' '
        AND   charg = i_charg
        AND   tcode = i_tcode.
    IF tg_lotes[] IS NOT INITIAL.
      PERFORM f_grava.
    ENDIF.
  ENDIF.



ENDFUNCTION.
