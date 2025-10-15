*&---------------------------------------------------------------------*
*& Report  ZFIR_CARTA_CORRECAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir_carta_correcao.

TABLES: j_1bnfdoc, j_1bnfe_active.

TYPES: BEGIN OF ty_file,
         docnum TYPE j_1bnfdoc-docnum,
         texto  TYPE string,
       END OF ty_file.

DATA: t_file TYPE TABLE OF ty_file.

SELECTION-SCREEN BEGIN OF BLOCK b1.

PARAMETERS p_arq TYPE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.

  PERFORM f_carrega_arq.

START-OF-SELECTION.

  PERFORM f_seleciona_arq.

  PERFORM f_seleciona_dados.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_arq .

  DATA: lv_caminho TYPE string.

  lv_caminho = p_arq.

  CLEAR t_file[].

  DATA: lt_raw TYPE truxs_t_text_data.

  DATA: lv_arquivo TYPE string.

  lv_arquivo = p_arq.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename = lv_arquivo
    CHANGING
      data_tab = lt_raw
    EXCEPTIONS
      OTHERS   = 1.

  IF lt_raw IS NOT INITIAL.
    DELETE lt_raw INDEX 1.

    CALL FUNCTION 'TEXT_CONVERT_TEX_TO_SAP'
      EXPORTING
        i_field_seperator    = ';'
        i_tab_raw_data       = lt_raw
      TABLES
        i_tab_converted_data = t_file
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc EQ 0.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_carrega_arq .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = 'P_ARQ'
      mask             = ',*.csv'
      mode             = 'O'
      title            = 'Caminho do Arquivo'
    IMPORTING
      filename         = p_arq
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: zcl_cce TYPE REF TO zcl_cce.
  DATA : vl_length     TYPE i,
         v_text_correc TYPE char1000sf.

  DATA : vl_id              TYPE zcarta_correcao-id_cc,
         ls_zcarta_correcao TYPE zcarta_correcao,
         tl_input_81        TYPE zsdt0081_t,
         wl_input_81        TYPE zsdt0081,
         wl_zsdt0080        TYPE zsdt0080.

  FREE: zcl_cce.
  CREATE OBJECT zcl_cce.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE @DATA(lt_j_1bnfe_active)
    FOR ALL ENTRIES IN @t_file
    WHERE docnum = @t_file-docnum       AND
          docsta  = '1'  AND
          cancel <> 'X'  AND
          scssta <> '2'  AND
          action_requ <> ''.

  IF sy-subrc = 0.
    SORT t_file BY docnum.
    LOOP AT lt_j_1bnfe_active INTO DATA(ls_j_1bnfe_active).

      READ TABLE t_file INTO DATA(ls_file) WITH KEY docnum = ls_j_1bnfe_active-docnum
                                                             BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        IF  ls_j_1bnfe_active-model = '57'.

          SELECT SINGLE MAX( id_cc )
            INTO vl_id
            FROM zcarta_correcao
           WHERE docnum = ls_j_1bnfe_active-docnum.

          IF vl_id IS INITIAL .
            vl_id  = 0.
          ENDIF.

          vl_id = vl_id + 1.

          MOVE: sy-mandt                  TO wl_input_81-mandt,
                ls_j_1bnfe_active-docnum  TO wl_input_81-docnum,
                vl_id                     TO wl_input_81-id_cc,
                'compl'                   TO wl_input_81-grupo,
                'xObs'                    TO wl_input_81-campo,
                ls_file-texto             TO wl_input_81-valor.
          APPEND wl_input_81 TO tl_input_81.

          TRY .

              zcl_cce->novo_registro( ).
              zcl_cce->set_docnum( ls_j_1bnfe_active-docnum ).
              zcl_cce->set_campos_correcao_cte( i_zsdt0081_t = tl_input_81 ).
              zcl_cce->gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

            CATCH zcx_integracao INTO DATA(ex_integra).

          ENDTRY.

          CLEAR: tl_input_81, wl_input_81.
        ENDIF.

        IF ls_j_1bnfe_active-model = '55'.

          TRY .

              v_text_correc = ls_file-texto.
              zcl_cce->novo_registro( ).
              zcl_cce->set_docnum( ls_j_1bnfe_active-docnum ).
              zcl_cce->set_texto_correcao( v_text_correc ).
              zcl_cce->gravar_registro( RECEIVING i_gravou = DATA(_gravou1) ).

            CATCH zcx_integracao INTO DATA(ex_integra2).

          ENDTRY.

          CLEAR: v_text_correc.
        ENDIF.


      ENDIF.

    ENDLOOP.

  ENDIF.
ENDFORM.
