*&---------------------------------------------------------------------*
*& Report  ZCOR023
*&
*&---------------------------------------------------------------------*
*& Carga de dados reservas de materiais para centro de custo empresa 0050 - Otelhar, retroativo ( Interface SAP x GEO )
*& ABAP: Anderson Oenning wsb

*& Data: 10/01/2022
*&---------------------------------------------------------------------*
REPORT zcor023.

TABLES: mkpf, mseg, t001k.

DATA: t_mseg      TYPE TABLE OF mseg,
      t_t001k     TYPE TABLE OF t001k,
      it_mseg     TYPE TABLE OF mseg,
      wa_mkpf     TYPE mkpf,
      wa_zcot0014 TYPE zcot0014,
      wa_mseg     TYPE mseg,
      t_mkpf      TYPE TABLE OF mkpf,
      t_zcot0014  TYPE TABLE OF zcot0014,
      r_werks     TYPE RANGE OF werks_d,
      r_mblnr     TYPE RANGE OF mblnr,
      r_bwart     TYPE RANGE OF bwart.


SELECTION-SCREEN: BEGIN OF BLOCK block1.
SELECT-OPTIONS: p_bukrs FOR t001k-bukrs OBLIGATORY,
                p_peri  FOR mkpf-budat OBLIGATORY,
                p_doc   FOR mkpf-mblnr.
SELECTION-SCREEN: END OF BLOCK block1.



START-OF-SELECTION.
  PERFORM f_seleciona_dados.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .


  APPEND  VALUE #( low = '201' sign = 'I' option = 'EQ'  ) TO r_bwart.
  APPEND  VALUE #( low = '202' sign = 'I' option = 'EQ'  ) TO r_bwart.

  SELECT * FROM t001k INTO TABLE t_t001k WHERE bukrs IN p_bukrs.
  CHECK t_t001k IS NOT INITIAL.
  r_werks = VALUE #( FOR l IN  t_t001k ( low = l-bwkey sign = 'I' option = 'EQ' ) ).



  IF p_doc IS NOT INITIAL.
    SELECT * FROM mseg AS a
    INTO CORRESPONDING FIELDS OF TABLE t_mseg
    WHERE a~bwart IN r_bwart
    AND a~budat_mkpf IN p_peri
    AND a~werks IN r_werks
    AND a~mblnr IN p_doc.
  ELSE.
    SELECT * FROM mseg AS a
    INTO CORRESPONDING FIELDS OF TABLE t_mseg
    WHERE a~bwart IN r_bwart
    AND a~budat_mkpf IN p_peri
    AND a~werks IN r_werks
    AND a~mblnr NE space.
  ENDIF.

  CHECK t_mseg IS NOT INITIAL.
  SORT t_mseg BY mblnr.

  SELECT * FROM zcot0014 INTO TABLE t_zcot0014
  FOR ALL ENTRIES IN t_mseg WHERE mblnr EQ t_mseg-mblnr AND rsnum EQ t_mseg-rsnum AND rspos EQ t_mseg-rspos.

*  IF r_mblnr IS NOT INITIAL.
*    "Deleta documento ja processados.
*    DELETE t_mseg WHERE mblnr IN r_mblnr.
*  ENDIF.

  CHECK t_mseg IS NOT INITIAL.

  SELECT * FROM mkpf INTO TABLE t_mkpf FOR ALL ENTRIES IN t_mseg WHERE mblnr EQ t_mseg-mblnr.

  CHECK t_mkpf IS NOT INITIAL.

  PERFORM f_processa_dados .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  DATA: ztext TYPE string.

  LOOP AT t_mseg INTO wa_mseg.
    READ TABLE t_zcot0014 INTO DATA(w_zcot0014) WITH KEY mblnr = wa_mseg-mblnr
                                                         rsnum = wa_mseg-rsnum
                                                         rspos = wa_mseg-rspos.
    IF w_zcot0014 IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    ztext = | Processando documento { wa_mseg-mblnr } |.
    PERFORM f_progress USING 95 ztext.

    READ TABLE t_mkpf INTO wa_mkpf WITH KEY mblnr = wa_mseg-mblnr.
    IF sy-subrc EQ 0.
      IF wa_mkpf-bktxt IS NOT INITIAL.
        wa_mkpf-xblnr = wa_mkpf-bktxt+0(16).
      ENDIF.

      "Executa a função interface SAP x GEO.
      CALL FUNCTION 'Z_MM_PREPARE_OUTBOUND_OS' IN BACKGROUND TASK
        DESTINATION 'NONE'
        EXPORTING
          i_mkpf = wa_mkpf
          i_mseg = wa_mseg.

      " Registra documento processados.
      wa_zcot0014-mblnr = wa_mseg-mblnr.
      wa_zcot0014-rsnum = wa_mseg-rsnum.
      wa_zcot0014-rspos = wa_mseg-rspos.

      MODIFY zcot0014 FROM wa_zcot0014.
      COMMIT WORK.
    ENDIF.

    CLEAR: wa_mkpf, w_zcot0014.
  ENDLOOP.

ENDFORM.

FORM f_progress  USING vf_percen vf_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = vf_percen   " Veloc do relogio em %
      text       = vf_text.    " Texto que aparecerá.
ENDFORM.
