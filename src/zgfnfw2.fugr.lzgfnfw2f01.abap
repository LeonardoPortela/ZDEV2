*----------------------------------------------------------------------*
***INCLUDE LZGFNFWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH: tg_fieldcatalog.

  PERFORM montar_estrutura USING:
1  ' ' ' '    'IT_MSGS'   'MSG'      'Mensagem de Erro'  ' '.
ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)     TYPE i.



  CLEAR: wg_fieldcatalog.

  wg_fieldcatalog-fieldname     = p_field.
  wg_fieldcatalog-tabname       = p_tabname.
  wg_fieldcatalog-ref_table     = p_ref_tabname.
  wg_fieldcatalog-ref_field     = p_ref_fieldname.
  wg_fieldcatalog-key           = ' '.
  wg_fieldcatalog-key_sel       = 'X'.
  wg_fieldcatalog-col_pos       = p_col_pos.
  wg_fieldcatalog-no_out        = ' '.
  wg_fieldcatalog-scrtext_l     = p_scrtext_l.
  wg_fieldcatalog-scrtext_m     = p_scrtext_l.
  wg_fieldcatalog-scrtext_s     = p_scrtext_l.
  wg_fieldcatalog-outputlen     = p_outputlen.



  IF wg_fieldcatalog-scrtext_l NE space.

    wg_fieldcatalog-selddictxt       = 'L'.

  ENDIF.

  APPEND wg_fieldcatalog TO tg_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
*----------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro
*&---------------------------------------------------------------------*
*&      Form  TRANSFERE_VALOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_2002  text
*----------------------------------------------------------------------*
FORM transfere_valor  USING    tl_2002 TYPE zfiwrt2002.

  DATA: wl_field(60).
  DATA: wl_field_nfenum(60),
        wl_field_series(60),
        wl_field_nota(60).



  IF tl_2002-par_name2 IS NOT INITIAL.
    CONCATENATE tl_2002-par_name tl_2002-par_name2 INTO wl_field SEPARATED BY '-' .
  ELSE.
    MOVE tl_2002-par_name TO wl_field.
  ENDIF.
  ASSIGN (wl_field) TO <fs_field>.
  IF tl_2002-par_fixo EQ 'S'.
    ASSIGN tl_2002-par_value2 TO <fs_field2>.
  ELSE.

    CASE wl_field.

      WHEN: 'GOODSMVT_HEADER-REF_DOC_NO'.
        IF tl_2002-par_value2 NE 'MTSNR'.
          CONCATENATE tl_2002-par_value tl_2002-par_value2 INTO wl_field SEPARATED BY '-'.
          ASSIGN (wl_field) TO <fs_nfenum>.

          CONCATENATE tl_2002-par_value 'SERIES' INTO wl_field SEPARATED BY '-'.
          ASSIGN (wl_field) TO <fs_series>.

          wl_field_nfenum = <fs_nfenum>.
          SHIFT wl_field_nfenum LEFT DELETING LEADING '0'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <fs_series>
            IMPORTING
              output = wl_field_series.

          CONCATENATE wl_field_nfenum wl_field_series INTO wl_field_nota SEPARATED BY '-'.
          ASSIGN ('WL_FIELD_NOTA') TO <fs_field2>.
        ELSE.
          CONCATENATE tl_2002-par_value tl_2002-par_value2 INTO wl_field SEPARATED BY '-'.
          ASSIGN (wl_field) TO <fs_field2>.
        ENDIF.
      WHEN OTHERS.
        CONCATENATE tl_2002-par_value tl_2002-par_value2 INTO wl_field SEPARATED BY '-'.
        ASSIGN (wl_field) TO <fs_field2>.
    ENDCASE.


  ENDIF.
  <fs_field> = <fs_field2>.
ENDFORM.                    " TRANSFERE_VALOR

*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Início de Alteração
FORM atribuir_valor_zfiwrs0005 USING zfiwrt0009 TYPE zfiwrt0009.

  CLEAR zfiwrs0005.

  PERFORM atribuir_imposto USING zfiwrt0009.

ENDFORM.

FORM atribuir_imposto USING zfiwrt0009 TYPE zfiwrt0009.

  DATA:
        vl_icm3 TYPE zfiwrt0010-taxtyp VALUE 'ICM3'.

  READ TABLE t_zfiwrt0010 INTO DATA(wl_zfiwrt0010)
                          WITH KEY seq_lcto = zfiwrt0009-seq_lcto
                                   itmnum   = zfiwrt0009-itmnum
                                   taxtyp   = vl_icm3
                                                    BINARY SEARCH.
  IF sy-subrc IS INITIAL
  AND wl_zfiwrt0010-taxval NE 0.

    zfiwrs0005-mwskz = 'I8'.

  ELSE.

    SELECT SINGLE taxval
      INTO @DATA(lv_taxval)
      FROM zfiwrt0010
     WHERE seq_lcto = @zfiwrt0009-seq_lcto
       AND itmnum   = @zfiwrt0009-itmnum
       AND taxtyp   = @vl_icm3.

    IF lv_taxval IS NOT INITIAL.
      zfiwrs0005-mwskz = 'I8'.
    ENDIF.

  ENDIF.

ENDFORM.
*** Stefanini - IR237366 - 29/05/2025 - LAZAROSR - Fim de Alteração
