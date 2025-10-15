*----------------------------------------------------------------------*
***INCLUDE LZLES_FRETE_AQUAV_CONTINGENF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_verifica_duplicidade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_selecao_hana.

  CHECK  t_zlest0056_ecc[] IS NOT INITIAL.

  SELECT *
    FROM zlest0056
    INTO TABLE t_zlest0056_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0057
    INTO TABLE t_zlest0057_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0058
    INTO TABLE t_zlest0058_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0060
    INTO TABLE t_zlest0060_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0061
    INTO TABLE t_zlest0061_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0063
    INTO TABLE t_zlest0063_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0068
    INTO TABLE t_zlest0068_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0076
    INTO TABLE t_zlest0076_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE bukrs          = t_zlest0056_ecc-bukrs
     AND werks          = t_zlest0056_ecc-werks
     AND nr_viagem      = t_zlest0056_ecc-nr_viagem
     AND ano_viagem     = t_zlest0056_ecc-ano_viagem.

  SELECT *
    FROM zlest0166
    INTO TABLE t_zlest0166_hana
     FOR ALL ENTRIES IN t_zlest0056_ecc
   WHERE empresa        = t_zlest0056_ecc-bukrs
     AND centro         = t_zlest0056_ecc-werks
     AND viagem         = t_zlest0056_ecc-nr_viagem
     AND ano            = t_zlest0056_ecc-ano_viagem.

  IF t_zlest0060_hana[] IS NOT INITIAL.
    SELECT *
      FROM zlest0073
      INTO TABLE t_zlest0073_hana
       FOR ALL ENTRIES IN t_zlest0060_hana
     WHERE chave_nfe      = t_zlest0060_hana-chave_nfe.

    SELECT *
      FROM zlest0205
      INTO TABLE t_zlest0205_hana
       FOR ALL ENTRIES IN t_zlest0060_hana
     WHERE chave_nfe      = t_zlest0060_hana-chave_nfe.

    SELECT *
      FROM zlest0158
      INTO TABLE t_zlest0158_hana
       FOR ALL ENTRIES IN t_zlest0060_hana
     WHERE ch_referencia  = t_zlest0060_hana-ch_referencia.
  ENDIF.

ENDFORM.

FORM f_verifica_duplicidade .

  FREE: t_alv.

  LOOP AT t_zlest0056_ecc INTO w_zlest0056_ecc.
    READ TABLE  t_zlest0056_hana INTO w_zlest0056_hana WITH KEY bukrs      = w_zlest0056_ecc-bukrs
                                                                werks      = w_zlest0056_ecc-werks
                                                                nr_viagem  = w_zlest0056_ecc-nr_viagem
                                                                ano_viagem = w_zlest0056_ecc-ano_viagem.

    IF sy-subrc = 0.
      APPEND w_zlest0056_hana TO t_alv.
    ENDIF.
  ENDLOOP.

  IF t_alv[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há duplicidade de registros!'.
    EXIT.
  ENDIF.

  l_program                  = sy-repid.
  l_grid_title               = 'Registros que ja Existem no HANA'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_program
      i_structure_name       = 'zlest0056'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = l_program
      is_layout             = w_layout
      it_fieldcat           = t_fieldcat
      i_grid_title          = l_grid_title
      i_screen_start_column = 10
      i_screen_start_line   = 02
      i_screen_end_column   = 182
      i_screen_end_line     = 20
    TABLES
      t_outtab              = t_alv
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.

FORM f_carrega_tabelas.

  t_zlest0056[] =  t_zlest0056_ecc[].

  LOOP AT t_zlest0056 INTO w_zlest0056.

    FREE: t_tabl,
          t_0056_ecc,  t_0057_ecc,  t_0058_ecc,  t_0060_ecc,  t_0061_ecc,  t_0063_ecc,  t_0068_ecc,  t_0073_ecc,
          t_0076_ecc,  t_0158_ecc,  t_0166_ecc,  t_0205_ecc,
          t_0056_hana, t_0057_hana, t_0058_hana, t_0060_hana, t_0061_hana, t_0063_hana, t_0068_hana, t_0073_hana,
          t_0076_hana, t_0158_hana, t_0166_hana, t_0205_hana.

    PERFORM f_verif_tabela TABLES t_0056_ecc t_0056_hana USING 'ZLEST0056'.
    PERFORM f_verif_tabela TABLES t_0057_ecc t_0057_hana USING 'ZLEST0057'.
    PERFORM f_verif_tabela TABLES t_0058_ecc t_0058_hana USING 'ZLEST0058'.
    PERFORM f_verif_tabela TABLES t_0060_ecc t_0060_hana USING 'ZLEST0060'.
    PERFORM f_verif_tabela TABLES t_0061_ecc t_0061_hana USING 'ZLEST0061'.
    PERFORM f_verif_tabela TABLES t_0063_ecc t_0063_hana USING 'ZLEST0063'.
    PERFORM f_verif_tabela TABLES t_0068_ecc t_0068_hana USING 'ZLEST0068'.
    PERFORM f_verif_tabela TABLES t_0076_ecc t_0076_hana USING 'ZLEST0076'.
    PERFORM f_verif_tabela TABLES t_0166_ecc t_0166_hana USING 'ZLEST0166'.
    PERFORM f_verif_tabela TABLES t_0073_ecc t_0073_hana USING 'ZLEST0073'.
    PERFORM f_verif_tabela TABLES t_0205_ecc t_0205_hana USING 'ZLEST0205'.
    PERFORM f_verif_tabela TABLES t_0158_ecc t_0158_hana USING 'ZLEST0158'.

    PERFORM f_grava_tabelas.

  ENDLOOP.

ENDFORM.

FORM f_verif_tabela  TABLES t_saida1
                            t_saida2
                      USING p_tabela.

  DATA: l_name1     TYPE char50,
        l_name2     TYPE char50,
        l_naotem_nf TYPE char01.

  FREE: t_saida1, t_saida2, l_naotem_nf.

  l_name1  = 'T_' && p_tabela && '_ECC'.
  l_name2  = 'T_' && p_tabela && '_HANA'.

  ASSIGN (l_name1)  TO <t_tab_ecc>.
  ASSIGN (l_name2)  TO <t_tab_hana>.

  CASE p_tabela.
    WHEN 'ZLEST0073' OR 'ZLEST0205'.
      l_where1 = 'chave_nfe = w_0060_ecc-chave_nfe'.
      l_where2 = 'chave_nfe = w_0060_hana-chave_nfe'.

      LOOP AT t_0060_ecc INTO w_0060_ecc.
        LOOP AT <t_tab_ecc> ASSIGNING <w_tab_ecc> WHERE (l_where1).
          APPEND <w_tab_ecc>  TO t_saida1.
        ENDLOOP.
      ENDLOOP.

      LOOP AT t_0060_hana INTO w_0060_hana.
        LOOP AT <t_tab_hana> ASSIGNING <w_tab_hana> WHERE (l_where2).
          APPEND <w_tab_hana>  TO t_saida2.
        ENDLOOP.
      ENDLOOP.

      IF t_saida1[] <> t_saida2[].
        APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
      ENDIF.

    WHEN 'ZLEST0158'.
      l_where1 = 'ch_referencia = w_0060_ecc-ch_referencia'.
      l_where2 = 'ch_referencia = w_0060_hana-ch_referencia'.

      LOOP AT t_0060_ecc INTO w_0060_ecc.
        LOOP AT <t_tab_ecc> ASSIGNING <w_tab_ecc> WHERE (l_where1).
          APPEND <w_tab_ecc>  TO t_saida1.
        ENDLOOP.
      ENDLOOP.

      LOOP AT t_0060_hana INTO w_0060_hana.
        LOOP AT <t_tab_hana> ASSIGNING <w_tab_hana> WHERE (l_where2).
          APPEND <w_tab_hana>  TO t_saida2.
        ENDLOOP.

        IF t_saida1[] <> t_saida2[].
          APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
        ENDIF.
      ENDLOOP.

    WHEN 'ZLEST0058'.
      l_where  = 'bukrs = w_zlest0056-bukrs AND werks = w_zlest0056-werks AND ano_viagem  = w_zlest0056-ano_viagem'.

      LOOP AT <t_tab_ecc> ASSIGNING <w_tab_ecc> WHERE (l_where).
        APPEND <w_tab_ecc>  TO t_saida1.
      ENDLOOP.

      LOOP AT <t_tab_hana> ASSIGNING <w_tab_hana> WHERE (l_where).
        APPEND <w_tab_hana>  TO t_saida2.
      ENDLOOP.

      IF t_saida1[] <> t_saida2[].
        APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
      ENDIF.

    WHEN 'ZLEST0166'.
      l_where  = 'empresa = w_zlest0056-bukrs AND centro = w_zlest0056-werks AND viagem = w_zlest0056-nr_viagem AND ano = w_zlest0056-ano_viagem'.

      LOOP AT <t_tab_ecc> ASSIGNING <w_tab_ecc> WHERE (l_where).
        APPEND <w_tab_ecc>  TO t_saida1.
      ENDLOOP.

      LOOP AT <t_tab_hana> ASSIGNING <w_tab_hana> WHERE (l_where).
        APPEND <w_tab_hana>  TO t_saida2.
      ENDLOOP.

      IF t_saida1[] <> t_saida2[].
        APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
      ENDIF.

    WHEN OTHERS.
      l_where  = 'bukrs = w_zlest0056-bukrs AND werks = w_zlest0056-werks AND nr_viagem = w_zlest0056-nr_viagem AND ano_viagem  = w_zlest0056-ano_viagem'.

      LOOP AT <t_tab_ecc> ASSIGNING <w_tab_ecc> WHERE (l_where).
        IF p_tabela = 'ZLEST0056'.
          ASSIGN COMPONENT 'FAT_CONTINGENCIA_ECC' OF STRUCTURE <w_tab_ecc> TO <f_contingencia>.
          <f_contingencia> = abap_false.
        ENDIF.

        IF p_tabela = 'ZLEST0060' OR p_tabela = 'ZLEST0061'.
          ASSIGN COMPONENT 'DOCNUM'               OF STRUCTURE <w_tab_ecc> TO <f_docnum>.
          ASSIGN COMPONENT 'FAT_CONTINGENCIA_ECC' OF STRUCTURE <w_tab_ecc> TO <f_contingencia>.

          SELECT SINGLE docnum
                   FROM j_1bnfdoc
                   INTO @DATA(l_docnum)
                  WHERE docnum = @<f_docnum>
                    AND model  = '57'.
          IF sy-subrc <> 0.
            l_naotem_nf = abap_true.
            IF p_tabela = 'ZLEST0060'.
              CLEAR <f_docnum>.
              <f_contingencia> = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND <w_tab_ecc>  TO t_saida1.
      ENDLOOP.

      LOOP AT <t_tab_hana> ASSIGNING <w_tab_hana> WHERE (l_where).
        IF p_tabela = 'ZLEST0056'.
          ASSIGN COMPONENT 'FAT_CONTINGENCIA_ECC' OF STRUCTURE <w_tab_hana> TO <f_contingencia>.
          <f_contingencia> = abap_false.
        ENDIF.

*        IF p_tabela = 'ZLEST0060' OR p_tabela = 'ZLEST0061'.
*          ASSIGN COMPONENT 'DOCNUM'               OF STRUCTURE <w_tab_hana> TO <f_docnum>.
*          ASSIGN COMPONENT 'FAT_CONTINGENCIA_ECC' OF STRUCTURE <w_tab_hana> TO <f_contingencia>.
*
*          SELECT SINGLE docnum
*                   FROM j_1bnfdoc
*                   INTO @DATA(l_docnum2)
*                  WHERE docnum = @<f_docnum>.
*          IF sy-subrc <> 0.
*            CLEAR <f_docnum>.
*            <f_contingencia> = abap_true.
*          ENDIF.
*        ENDIF.

        APPEND <w_tab_hana>  TO t_saida2.
      ENDLOOP.

      IF p_tabela = 'ZLEST0061'.
        IF l_naotem_nf = abap_false.
          IF t_saida1[] <> t_saida2[].
            APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
          ENDIF.
        ENDIF.
      ELSE.
        IF t_saida1[] <> t_saida2[].
          APPEND VALUE #( tab_stand = p_tabela ) TO t_tabl.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.

FORM f_grava_tabelas.

  DATA: l_name1 TYPE char50,
        l_name2 TYPE char50.

  CHECK t_tabl[] IS NOT INITIAL.

  READ TABLE t_tabl INTO w_tabl WITH KEY tab_stand = 'ZLEST0056'.
  IF sy-subrc <> 0.
    APPEND VALUE #( tab_stand = 'ZLEST0056' ) TO t_tabl.
  ENDIF.

  LOOP AT t_0056_ecc             INTO w_0056_ecc.
    w_0056_ecc-fat_contingencia_ecc = abap_true.
    MODIFY t_0056_ecc            FROM w_0056_ecc INDEX sy-tabix.
  ENDLOOP.

  LOOP AT t_tabl INTO w_tabl.
    l_name1  = 'T_' && w_tabl-tab_stand+5(4) && '_ECC'.
    l_name2  = 'T_' && w_tabl-tab_stand+5(4) && '_HANA'.

    ASSIGN (l_name1)  TO <t_tab_ecc>.
    ASSIGN (l_name2)  TO <t_tab_hana>.

    PERFORM f_gera_backup         TABLES <t_tab_ecc>  USING 'ECC'  w_tabl-tab_stand.
    PERFORM f_gera_backup         TABLES <t_tab_hana> USING 'HANA' w_tabl-tab_stand.

    DELETE (w_tabl-tab_stand) FROM TABLE <t_tab_hana>.
    MODIFY (w_tabl-tab_stand) FROM TABLE <t_tab_ecc>.

    COMMIT WORK.
  ENDLOOP.

ENDFORM.

FORM f_gera_backup TABLES t_tabela
                    USING p_sistema
                          p_nometab.

  FIELD-SYMBOLS: <f_field> TYPE any.

  DATA: l_seq        TYPE numc10,
        l_dado(1000) TYPE c,
        l_valor(50)  TYPE c.

  CLEAR: w_zlest_frete_cont, l_seq.

  SELECT COUNT(*)
    FROM zlest_frete_cont
    INTO l_seq
   WHERE sistema    = p_sistema
     AND tabela     = p_nometab
     AND bukrs      = w_zlest0056-bukrs
     AND werks      = w_zlest0056-werks
     AND nr_viagem  = w_zlest0056-nr_viagem
     AND ano_viagem = w_zlest0056-ano_viagem.

  w_zlest_frete_cont-mandt      = sy-mandt.
  w_zlest_frete_cont-sistema    = p_sistema.
  w_zlest_frete_cont-tabela     = p_nometab.
  w_zlest_frete_cont-bukrs      = w_zlest0056-bukrs.
  w_zlest_frete_cont-werks      = w_zlest0056-werks.
  w_zlest_frete_cont-nr_viagem  = w_zlest0056-nr_viagem.
  w_zlest_frete_cont-ano_viagem = w_zlest0056-ano_viagem.
  w_zlest_frete_cont-data       = sy-datum.
  w_zlest_frete_cont-hora       = sy-uzeit.
  w_zlest_frete_cont-usuario    = sy-uname.

  LOOP AT t_tabela ASSIGNING <w_tabela>.
    l_seq  = l_seq + 1.
    l_dado = abap_off.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <w_tabela> TO <f_field>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      l_valor = <f_field>.
      l_dado  = l_dado && l_valor.
    ENDDO.

    w_zlest_frete_cont-seq      = l_seq.
    w_zlest_frete_cont-dados    = l_dado.

    MODIFY zlest_frete_cont  FROM w_zlest_frete_cont.
  ENDLOOP.

ENDFORM.
