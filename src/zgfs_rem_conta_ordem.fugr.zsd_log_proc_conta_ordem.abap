FUNCTION zsd_log_proc_conta_ordem.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_VBELN_VENDA STRUCTURE  ZLEST0212_OUT OPTIONAL
*"      T_VBELN_TRANSF STRUCTURE  ZLEST0214_OUT OPTIONAL
*"  EXCEPTIONS
*"      NO_LOG
*"----------------------------------------------------------------------

  FREE: t_logproc, r_name,
        ok_code,
        g_custom_container,
        g_grid.

  "CHECK t_vbeln_venda[] IS NOT INITIAL. - US - 92467 - CBRAND

*----------------------------------------------
*-Montar Range icons
*----------------------------------------------
  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_GREEN_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_YELLOW_LIGHT'.
  APPEND r_name.

  r_name-sign   = 'I'.
  r_name-option = 'EQ'.
  r_name-low    = 'ICON_RED_LIGHT'.
  APPEND r_name.

  SELECT id
         name
    FROM icon
    INTO TABLE t_icon
   WHERE name IN r_name.

  SORT t_icon  BY name.

*** US - 92467 - Inicio - CBRAND
  IF t_vbeln_venda[] IS NOT INITIAL.
*** US - 92467 - Fim - CBRAND
    SELECT *
      FROM zlest0214
      INTO TABLE t_zlest0214
       FOR ALL ENTRIES IN t_vbeln_venda
     WHERE   vbeln      = t_vbeln_venda-vbeln
       AND ( nf_venda   = t_vbeln_venda-nf_venda
        OR   nf_venda   = abap_off ).

    IF sy-subrc <> 0.
      RAISE no_log.
    ENDIF.

    LOOP AT t_zlest0214 INTO w_zlest0214.

      FREE: t_idd07v, w_idd07v, w_icon.

      MOVE-CORRESPONDING w_zlest0214  TO w_logproc.

      CASE w_zlest0214-msgtyp.
        WHEN 'S'.
          l_icon_name = 'ICON_GREEN_LIGHT'.
        WHEN 'W'.
          l_icon_name = 'ICON_YELLOW_LIGHT'.
        WHEN 'E'.
          l_icon_name = 'ICON_RED_LIGHT'.
      ENDCASE.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZETAPA_PROC'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = t_idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = w_zlest0214-etapa_proc.

      READ TABLE t_icon   INTO w_icon   WITH KEY name = l_icon_name
                                        BINARY SEARCH.

      MOVE w_icon-id                  TO w_logproc-status.
*   MOVE w_idd07v-ddtext            TO w_logproc-etapa_desc.
      w_logproc-etapa_desc             = w_zlest0214-etapa_proc && '-' && w_idd07v-ddtext.

      APPEND w_logproc                TO t_logproc.
    ENDLOOP.

    SORT t_logproc BY vbeln etapa_proc.

  ELSE.
    CHECK t_vbeln_transf[] IS NOT INITIAL.

    SELECT *
    FROM zlest0214
    INTO TABLE t_zlest0214
     FOR ALL ENTRIES IN t_vbeln_transf
   WHERE  vbeln  = t_vbeln_transf-vbeln
       AND ebeln = t_vbeln_transf-ebeln
       AND ebelp = t_vbeln_transf-ebelp.

    IF sy-subrc <> 0.
      RAISE no_log.
    ENDIF.

    LOOP AT t_zlest0214 INTO w_zlest0214.

      FREE: t_idd07v, w_idd07v, w_icon.

      MOVE-CORRESPONDING w_zlest0214 TO w_logproc.

      CASE w_zlest0214-msgtyp.
        WHEN 'S'.
          l_icon_name = 'ICON_GREEN_LIGHT'.
        WHEN 'W'.
          l_icon_name = 'ICON_YELLOW_LIGHT'.
        WHEN 'E'.
          l_icon_name = 'ICON_RED_LIGHT'.
      ENDCASE.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = 'ZETAPA_PROC'
          text           = 'X'
          langu          = sy-langu
        TABLES
          dd07v_tab      = t_idd07v
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2.

      READ TABLE t_idd07v INTO w_idd07v WITH KEY domvalue_l = w_zlest0214-etapa_proc.

      READ TABLE t_icon   INTO w_icon   WITH KEY name = l_icon_name
                                        BINARY SEARCH.

      MOVE w_icon-id                  TO w_logproc-status.
      w_logproc-etapa_desc             = w_zlest0214-etapa_proc && '-' && w_idd07v-ddtext.

      APPEND w_logproc                TO t_logproc.
    ENDLOOP.

    SORT t_logproc BY vbeln etapa_proc.

  ENDIF.


*-----------------------------
* exibe log
*-----------------------------
  CALL SCREEN 120 STARTING AT 46  7
                    ENDING AT 143 16.

ENDFUNCTION.
