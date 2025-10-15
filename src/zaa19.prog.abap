*&---------------------------------------------------------------------*
*& Report ZAA19
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaa19.
TABLES faat_doc_it.
"
TYPES: BEGIN OF ty_imb,
         bukrs    TYPE faat_doc_it-bukrs,
         anln1    TYPE faat_doc_it-anln1,
         anln2    TYPE faat_doc_it-anln2,
         afabe    TYPE faat_doc_it-afabe,
         zerar(1),
       END OF ty_imb.

DATA: it_anep        TYPE TABLE OF anep,
      it_anep_aux    TYPE TABLE OF anep,
      wa_anep        TYPE anep,
      wa_anep_aux    TYPE anep,
      wa_faat_doc_it TYPE faat_doc_it,
      vg_anbtr       TYPE anep-anbtr,
      tabix          TYPE sy-tabix,
      vg_awitem      TYPE faat_doc_it-awitem.

DATA: w_imb TYPE ty_imb,
      t_imb TYPE TABLE OF ty_imb.

DATA: vmsg(70).

DATA: t_excel  LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      t_excel2 LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS    : p_bukrs TYPE faat_doc_it-bukrs,
                  p_anln1 TYPE faat_doc_it-anln1,
                  p_anln2 TYPE faat_doc_it-anln2,
                  p_afabe TYPE faat_doc_it-afabe,
                  p_zerar TYPE faat_doc_it-xreversing.

SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_file TYPE rlgrap-filename DEFAULT ''.

SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.xls*,'
      mode             = 'O'
      title            = 'Busca de Arquivo'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

AT SELECTION-SCREEN OUTPUT.
  IF p_afabe IS NOT INITIAL.
    IF NOT ( '10_11_15_16_20_30_42_50' CS p_afabe ).
      MESSAGE 'Area de avaliação deve ser 10_11_15_16_20_30_42_50' TYPE 'I'.
      SET CURSOR FIELD 'P_AFABE' .
    ENDIF.
  ENDIF.
  IF p_file IS INITIAL.
    IF p_bukrs IS INITIAL OR
       p_anln1 IS INITIAL OR
       p_anln2 IS INITIAL OR
       p_afabe IS INITIAL.
      MESSAGE 'Informe o imobilizado' TYPE 'I'.
      SET CURSOR FIELD 'P_BUKRS' .
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_file IS NOT INITIAL. "se Arquivo
    PERFORM processa_arquivo.
  ELSEIF  ( ( '10_11_15_16_20_30_42_50' CS p_afabe )  ) AND
          p_bukrs IS NOT INITIAL AND
          p_anln1 IS NOT INITIAL.

    REFRESH it_anep.
    SELECT  *
      INTO TABLE it_anep
      FROM anep
      WHERE bukrs   =  p_bukrs
      AND   anln1   =  p_anln1
      AND   anln2   =  p_anln2
      AND   gjahr   =  '2024'
*      AND   bzdat   GE  '20231016'
      AND   afabe   =  '41'.
    IF it_anep[] IS INITIAL.
      MESSAGE 'Não encontrado valores para a área 41' TYPE 'I'.
    ENDIF.
    REFRESH it_anep_aux.
    it_anep_aux[] = it_anep[].
    SORT it_anep_aux BY bukrs anln1 anln2 belnr buzei.
    DELETE ADJACENT DUPLICATES FROM it_anep_aux COMPARING  bukrs anln1 anln2 belnr buzei.
    "
    LOOP AT it_anep_aux INTO wa_anep_aux.
      CLEAR wa_anep_aux-anbtr.
      CLEAR vg_anbtr.
      tabix = sy-tabix.
      LOOP AT it_anep INTO wa_anep WHERE bukrs = wa_anep_aux-bukrs
                                   AND   anln1 = wa_anep_aux-anln1
                                   AND   anln2 = wa_anep_aux-anln2
                                   AND   belnr = wa_anep_aux-belnr
                                   AND   buzei = wa_anep_aux-buzei.
        ADD wa_anep-anbtr TO wa_anep_aux-anbtr.
      ENDLOOP.
      MODIFY it_anep_aux FROM wa_anep_aux INDEX tabix TRANSPORTING anbtr.
      vg_awitem = wa_anep_aux-buzei.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_awitem
        IMPORTING
          output = vg_awitem.
      "
      SELECT SINGLE *
        INTO wa_faat_doc_it
        FROM faat_doc_it
         WHERE bukrs   =  wa_anep_aux-bukrs
          AND  anln1   =  wa_anep_aux-anln1
          AND  anln2   =  wa_anep_aux-anln2
          AND  awref   =  wa_anep_aux-belnr
          AND  awitem  =  vg_awitem
          AND  gjahr   =  '2024'
          AND  afabe   =  p_afabe.

      IF sy-subrc = 0.
        IF p_zerar = 'X'.
          CLEAR wa_anep_aux-anbtr.
        ENDIF.
        UPDATE faat_doc_it SET hsl = wa_anep_aux-anbtr
        WHERE bukrs   =  wa_anep_aux-bukrs
        AND   anln1   =  wa_anep_aux-anln1
        AND   anln2   =  wa_anep_aux-anln2
        AND   awref   =  wa_anep_aux-belnr
        AND   awitem  =  vg_awitem
        AND   gjahr   =  '2024'
        AND   afabe   =  p_afabe.
        COMMIT WORK.
        MESSAGE |Area de avaliação { p_afabe } do imobilizado  { p_anln1 } DOC { wa_anep-belnr } atualizada!| TYPE 'I'.
      ELSE.
        ADD wa_anep_aux-anbtr TO vg_anbtr.
      ENDIF.

    ENDLOOP.
    IF vg_anbtr  NE 0 AND p_zerar IS INITIAL.
      LOOP AT it_anep_aux INTO wa_anep.
        IF wa_anep-anbtr GT abs( vg_anbtr ).
          wa_anep-anbtr = wa_anep-anbtr + vg_anbtr.
          UPDATE faat_doc_it SET hsl = wa_anep-anbtr
            WHERE bukrs   =  wa_anep-bukrs
            AND   anln1   =  wa_anep-anln1
            AND   anln2   =  wa_anep-anln2
            AND   awref   =  wa_anep-belnr
            AND   awitem  =  wa_anep-buzei
            AND   gjahr   =  '2024'
            AND   afabe   =  p_afabe.
          COMMIT WORK.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*& Form processa_arquivo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processa_arquivo .
  REFRESH: t_excel, t_excel2.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 5
      i_end_row               = 10000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Atualizando Dados'.

  t_excel2[] = t_excel[].
  SORT t_excel2 BY row col.
  CLEAR: t_excel2, t_imb.
  LOOP AT t_excel.
    IF t_excel-row = t_excel2-row.
      CONTINUE.
    ENDIF.
    LOOP AT t_excel2 WHERE row = t_excel-row.
      CASE t_excel2-col.
        WHEN 1.
          w_imb-bukrs = t_excel2-value.
        WHEN 2.
          w_imb-anln1 = t_excel2-value.
        WHEN 3.
          w_imb-anln2 = t_excel2-value.
        WHEN 4.
          w_imb-afabe = t_excel2-value.
        WHEN 5.
          w_imb-zerar = t_excel2-value.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'Linha ' t_excel-row 'Imobilizado '  w_imb-anln1 INTO vmsg SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.
    "somente permitir atualizar Area de Avaliação igual a 11, 16 e 42
    IF NOT ( '10_11_15_16_20_30_42_50' CS w_imb-afabe ).
      WRITE: / |Area de avaliação inválida, imobilizado  { w_imb-anln1 } | .
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_imb-anln1
      IMPORTING
        output = w_imb-anln1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_imb-anln2
      IMPORTING
        output = w_imb-anln2.

    REFRESH it_anep.
    SELECT  *
      INTO TABLE it_anep
      FROM anep
      WHERE bukrs   =  w_imb-bukrs
      AND   anln1   =  w_imb-anln1
      AND   anln2   =  w_imb-anln2
      AND   gjahr   =  '2024'
*      AND   bzdat   GE  '20231016'
      AND   afabe   =  '41'.

    IF it_anep[] IS INITIAL.
      WRITE: / |Não encontrado valores para a área 41, imobilizado  { w_imb-anln1 } | .
      CONTINUE.
    ENDIF.

    REFRESH it_anep_aux.
    it_anep_aux[] = it_anep[].
    SORT it_anep_aux BY bukrs anln1 anln2 belnr buzei.
    DELETE ADJACENT DUPLICATES FROM it_anep_aux COMPARING  bukrs anln1 anln2 belnr buzei.
    "

    LOOP AT it_anep_aux INTO wa_anep_aux.
      CLEAR wa_anep_aux-anbtr.
      CLEAR vg_anbtr.
      tabix = sy-tabix.
      LOOP AT it_anep INTO wa_anep WHERE bukrs = wa_anep_aux-bukrs
                                   AND   anln1 = wa_anep_aux-anln1
                                   AND   anln2 = wa_anep_aux-anln2
                                   AND   belnr = wa_anep_aux-belnr
                                   AND   buzei = wa_anep_aux-buzei.
        ADD wa_anep-anbtr TO wa_anep_aux-anbtr.
      ENDLOOP.
      MODIFY it_anep_aux FROM wa_anep_aux INDEX tabix TRANSPORTING anbtr.

      vg_awitem = wa_anep_aux-buzei.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_awitem
        IMPORTING
          output = vg_awitem.
      SELECT SINGLE *
        INTO wa_faat_doc_it
        FROM faat_doc_it
         WHERE bukrs   =  wa_anep_aux-bukrs
          AND  anln1   =  wa_anep_aux-anln1
          AND  anln2   =  wa_anep_aux-anln2
          AND  awref   =  wa_anep_aux-belnr
          AND  awitem  =   vg_awitem
          AND  gjahr   =  '2024'
          AND  afabe   =  w_imb-afabe.

      IF sy-subrc = 0.
        IF w_imb-zerar = 'X'.
          CLEAR wa_anep_aux-anbtr.
        ENDIF.
        UPDATE faat_doc_it SET hsl = wa_anep_aux-anbtr
        WHERE bukrs   =  wa_anep_aux-bukrs
        AND   anln1   =  wa_anep_aux-anln1
        AND   anln2   =  wa_anep_aux-anln2
        AND   awref   =  wa_anep_aux-belnr
        AND   awitem  =  vg_awitem
        AND   gjahr   =  '2024'
        AND   afabe   =  w_imb-afabe.
        COMMIT WORK.
        WRITE: / |Area de avaliação { w_imb-afabe } do imobilizado  { w_imb-anln1 } DOC { wa_anep-belnr } atualizada!| .
      ELSE.
        ADD wa_anep_aux-anbtr TO vg_anbtr.
      ENDIF.
    ENDLOOP.
    "
    IF vg_anbtr  NE 0 AND w_imb-zerar IS INITIAL.
      LOOP AT it_anep_aux INTO wa_anep.
        IF wa_anep-anbtr GT abs( vg_anbtr ).
          wa_anep-anbtr = wa_anep-anbtr + vg_anbtr.
          UPDATE faat_doc_it SET hsl = wa_anep-anbtr
            WHERE bukrs   =  wa_anep-bukrs
            AND   anln1   =  wa_anep-anln1
            AND   anln2   =  wa_anep-anln2
            AND   awref   =  wa_anep-belnr
            AND   awitem  =  wa_anep-buzei
            AND   gjahr   =  '2024'
            AND   afabe   =  w_imb-afabe.
          COMMIT WORK.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "
  ENDLOOP.
ENDFORM.
