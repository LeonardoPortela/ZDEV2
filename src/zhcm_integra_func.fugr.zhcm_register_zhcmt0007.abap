FUNCTION zhcm_register_zhcmt0007.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_ZHCMT0007) TYPE  ZHCMT0007 OPTIONAL
*"     REFERENCE(I_ZHCMT0007_T) TYPE  ZHCMT0007_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: lit_zhcmt0007_gravar  TYPE ZHCMT0007_T.
  DATA: lit_zhcmt0007_current TYPE ZHCMT0007_T.

  CLEAR: lit_zhcmt0007_gravar[].

  IF i_zhcmt0007 IS NOT INITIAL.
    APPEND i_zhcmt0007 to lit_zhcmt0007_gravar[].
  ELSEIF i_zhcmt0007_T[] IS NOT INITIAL.
    lit_zhcmt0007_gravar = i_zhcmt0007_T[].
  ENDIF.

  CHECK lit_zhcmt0007_gravar[] IS NOT INITIAL.

  SELECT *
    from zhcmt0007 INTO TABLE lit_zhcmt0007_current
     FOR ALL ENTRIES IN lit_zhcmt0007_gravar
   WHERE pernr eq lit_zhcmt0007_gravar-pernr.

  SORT lit_zhcmt0007_current by pernr.

  LOOP AT lit_zhcmt0007_gravar ASSIGNING FIELD-SYMBOL(<fs_zhcmt0007_gravar>).

    DATA(lva_integra_legado) = abap_true.

    READ TABLE lit_zhcmt0007_current ASSIGNING FIELD-SYMBOL(<fs_zhcmt0007_current>) WITH KEY pernr = <fs_zhcmt0007_gravar>-pernr BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      <fs_zhcmt0007_gravar>-mandt               = <fs_zhcmt0007_current>-mandt.
      <fs_zhcmt0007_gravar>-int_sistemas_legado = <fs_zhcmt0007_current>-int_sistemas_legado.
      <fs_zhcmt0007_gravar>-dt_int_legado       = <fs_zhcmt0007_current>-dt_int_legado.
      <fs_zhcmt0007_gravar>-hr_int_legado       = <fs_zhcmt0007_current>-hr_int_legado.

      IF <fs_zhcmt0007_gravar> NE <fs_zhcmt0007_current>.
        CLEAR: <fs_zhcmt0007_gravar>-int_sistemas_legado,<fs_zhcmt0007_gravar>-dt_int_legado,<fs_zhcmt0007_gravar>-hr_int_legado.
      ENDIF.
    ELSE.
      CLEAR: <fs_zhcmt0007_gravar>-int_sistemas_legado,<fs_zhcmt0007_gravar>-dt_int_legado,<fs_zhcmt0007_gravar>-hr_int_legado.
    ENDIF.

  ENDLOOP.

  MODIFY zhcmt0007 from TABLE lit_zhcmt0007_gravar.



ENDFUNCTION.
