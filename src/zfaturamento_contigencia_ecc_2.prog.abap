*&---------------------------------------------------------------------*
*& Report ZFATURAMENTO_CONTIGENCIA_ECC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfaturamento_contigencia_ecc_2.

TABLES: zsdt0001, zmmt_ee_zgr.

CONSTANTS: c_0(1)  TYPE c                            VALUE '0',
           c_1(1)  TYPE c                            VALUE '1',
           c_01    TYPE bapi2017_gm_code             VALUE '01',
           c_02(2) TYPE c                            VALUE '02',
           c_03(2) TYPE c                            VALUE '03',
           c_04(2) TYPE c                            VALUE '04',
           c_08(2) TYPE c                            VALUE '08',
           c_06(2) TYPE c                            VALUE '06',
           c_07(2) TYPE c                            VALUE '07',
           c_09(2) TYPE c                            VALUE '09',
           c_10    TYPE zfie_ret_document-interface  VALUE '10',
           c_11    TYPE zfie_ret_document-interface  VALUE '11',
           c_12    TYPE zfie_ret_document-interface  VALUE '12',
           c_05    TYPE zfie_ret_document-interface  VALUE '05'.

SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_fixdm  RADIOBUTTON GROUP g1 USER-COMMAND usr1,
                  p_other  RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN: END   OF BLOCK b0.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: p_chref    FOR zsdt0001-ch_referencia   MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_branch   FOR zsdt0001-branch          MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_safra    FOR zsdt0001-nr_safra        MODIF ID t2 NO INTERVALS NO-EXTENSION, " OBLIGATORY,
                  p_nrrom    FOR zsdt0001-nr_romaneio     MODIF ID t2, " OBLIGATORY,
                  p_dtmov    FOR zsdt0001-dt_movimento    MODIF ID t2, " OBLIGATORY,
                  p_tpmov    FOR zsdt0001-tp_movimento    MODIF ID t2 NO INTERVALS, " OBLIGATORY,
                  p_stproc   FOR zsdt0001-st_proc         MODIF ID t2 NO INTERVALS  NO-EXTENSION, " OBLIGATORY,
                  p_objee    FOR zmmt_ee_zgr-obj_key      MODIF ID t2 NO INTERVALS. " OBLIGATORY,

SELECTION-SCREEN: END OF BLOCK b3.




START-OF-SELECTION.


  PERFORM f_processar.

FORM f_processar .

  CASE abap_true.
    WHEN p_fixdm.
      PERFORM f_fix_doc_material_zarm.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_fix_doc_material_zarm.

  SELECT *
    FROM ZSDT0001 INTO TABLE @DATA(LIT_ZSDT0001_ARM)
   WHERE fat_contingencia_ecc = @abap_true.

  delete LIT_ZSDT0001_ARM where doc_rem is NOT INITIAL.
  delete LIT_ZSDT0001_ARM where doc_material is INITIAL.
  delete LIT_ZSDT0001_ARM where vbeln is INITIAL.

  CHECK LIT_ZSDT0001_ARM[] is NOT INITIAL.

  SELECT *
    from ekko INTO TABLE @DATA(lit_ekko)
     FOR ALL ENTRIES IN @LIT_ZSDT0001_ARM
   WHERE ebeln = @LIT_ZSDT0001_ARM-vbeln.

  delete lit_ekko WHERE bsart <> 'ZARM'.

  DATA(LIT_ZSDT0001_ARM_aux) = LIT_ZSDT0001_ARM[].

  LOOP AT LIT_ZSDT0001_ARM_aux INTO DATA(lwa_zsdt0001_aux).
    READ TABLE LIT_EKKO INTO DATA(LWA_EKKO) WITH KEY EBELN = lwa_zsdt0001_aux-ebeln.
    IF SY-SUBRC NE 0.
      DELETE LIT_ZSDT0001_ARM WHERE CH_REFERENCIA = lwa_zsdt0001_aux-ch_referencia.
    ENDIF.
  ENDLOOP.

  LOOP AT LIT_ZSDT0001_ARM INTO DATA(lwa_zsdt0001_ZARM).
    SELECT SINGLE *
      FROM MKPF INTO @DATA(LWA_MKPF)
     WHERE MBLNR          EQ @lwa_zsdt0001_ZARM-doc_material
       AND ZCH_REFERENCIA EQ @lwa_zsdt0001_ZARM-ch_referencia.

    IF SY-SUBRC NE 0.
      CLEAR: lwa_zsdt0001_ZARM-doc_material, lwa_zsdt0001_ZARM-ano_material, lwa_zsdt0001_ZARM-status.
      MODIFY ZSDT0001 FROM lwa_zsdt0001_ZARM.
    ENDIF.
  ENDLOOP.

ENDFORM.
