************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 10.06.2010                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Altera dados de nota fiscal eltrônica com erros     *
*                                                                      *
************************************************************************

REPORT  znfeinfo.

DATA: BEGIN OF t_log2 OCCURS 0.
        INCLUDE STRUCTURE zfie_ret_document.
DATA: END OF t_log2.

DATA: BEGIN OF wa_nfe.
        INCLUDE STRUCTURE znfeinfo.
DATA: END OF wa_nfe.

DATA:  t_nfe   LIKE STANDARD TABLE OF wa_nfe.

CONSTANTS: vg_begin_col  TYPE i VALUE 1,
           vg_begin_row  TYPE i VALUE 2,
           vg_end_col    TYPE i VALUE 10,
           vg_end_row    TYPE i VALUE 64000.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s02.
PARAMETER:
          r_normal  RADIOBUTTON GROUP tp DEFAULT 'X',
          r_arquiv  RADIOBUTTON GROUP tp.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s01.
PARAMETERS:
      p_objkey LIKE znfeinfo-obj_key,
      p_docnum LIKE j_1bnfdoc-docnum,
      p_nfenum LIKE j_1bnfdoc-nfenum,
      p_docsta LIKE j_1bnfdoc-docstat DEFAULT '1',
      p_code   LIKE j_1bnfe_active-code DEFAULT '100',
      p_cdv    LIKE j_1bnfe_active-cdv,
      p_scssta LIKE j_1bnfe_active-scssta DEFAULT '2',
      p_tpemis LIKE j_1bnfe_active-tpemis.

SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s02.
PARAMETERS:
          p_arq         LIKE rlgrap-filename.  "Arq APLIC.SERVER
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN END   OF BLOCK b1.


*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK b0.

  IF NOT r_normal IS INITIAL.

    IF p_objkey IS INITIAL.
      MESSAGE 'Deve ser informado a planilha do registro fiscal no SIGAM!' TYPE 'E'.
    ENDIF.
    IF p_docnum IS INITIAL.
      MESSAGE 'Deve ser informado o número do documento SAP!' TYPE 'E'.
    ENDIF.
    IF p_nfenum IS INITIAL.
      MESSAGE 'Deve ser informado o número do documento fiscal!' TYPE 'E'.
    ENDIF.
    IF p_docsta IS INITIAL.
      MESSAGE 'Deve ser informado o status do documento fiscal!' TYPE 'E'.
    ENDIF.
    IF p_code   IS INITIAL.
      MESSAGE 'Deve ser informado o código SEFAZ do documento fiscal!' TYPE 'E'.
    ENDIF.
    IF p_cdv    IS INITIAL.
      MESSAGE 'Deve ser informado o dígito verificador do documento fiscal!' TYPE 'E'.
    ENDIF.
    IF p_scssta IS INITIAL.
      MESSAGE 'Deve ser informado o status de comunicação do sistema!' TYPE 'E'.
    ENDIF.
    IF p_tpemis IS INITIAL.
      MESSAGE 'Deve ser informado a forma de emissão!' TYPE 'E'.
    ENDIF.
    SELECT SINGLE docnum INTO p_docnum
      FROM j_1bnfdoc
     WHERE docnum = p_docnum.

    IF sy-subrc NE 0.
      MESSAGE 'Número de documento não encontrado!' TYPE 'E'.
    ENDIF.

  ENDIF.

*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK b2.

  IF r_normal IS INITIAL.
    IF p_arq IS INITIAL.
      CALL FUNCTION 'WS_FILENAME_GET'
        EXPORTING
          def_path         = p_arq
          mode             = 'O'
          title            = 'Diretório do arquivo de Entrada'
        IMPORTING
          filename         = p_arq
        EXCEPTIONS
          inv_winsys       = 1
          no_batch         = 2
          selection_cancel = 3
          selection_error  = 4
          OTHERS           = 5.
      IF sy-subrc NE 0.
        MESSAGE 'Erro so selecionar arquivo!' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA : v_nfenum    LIKE znfeinfo-nfenum,
         p_arquivo   LIKE filename-fileextern,
         wa_planilha TYPE alsmex_tabline,
         it_planilha LIKE STANDARD TABLE OF wa_planilha.

  IF r_normal EQ 'X'.
    " Se Informado manual
    CLEAR: wa_nfe, t_nfe.
    wa_nfe-obj_key = p_objkey.
    wa_nfe-docnum  = p_docnum.
    wa_nfe-nfenum  = p_nfenum.
    wa_nfe-docstat = p_docsta.
    wa_nfe-docsta  = p_docsta.
    wa_nfe-code    = p_code.
    wa_nfe-nfnum9  = p_nfenum.
    wa_nfe-cdv     = p_cdv.
    wa_nfe-scssta  = p_scssta.
    wa_nfe-tpemis  = p_tpemis.
    APPEND wa_nfe TO t_nfe.
  ELSE.
    " Se Informado arquivo

    "Caregar os dados de uma planilha em uma tabela interna
    REFRESH it_planilha.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_arq
        i_begin_col             = vg_begin_col
        i_begin_row             = vg_begin_row
        i_end_col               = vg_end_col
        i_end_row               = vg_end_row
      TABLES
        intern                  = it_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      DELETE FROM zgl013_espera.
      MESSAGE e000(z01) WITH 'Problema ao carregar o arquivo XLS'.
    ENDIF.

    LOOP AT it_planilha INTO wa_planilha.

      AT NEW row.
        CLEAR: wa_nfe.
      ENDAT.

      CASE wa_planilha-col.
        WHEN 2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-obj_key.
        WHEN 3.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-docnum.
        WHEN 4.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-nfenum.
        WHEN 5.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-docstat.
        WHEN 6.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-docsta.
        WHEN 7.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-code.
        WHEN 8.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-nfnum9.
        WHEN 9.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-cdv.
        WHEN 10.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_planilha-value
            IMPORTING
              output = wa_nfe-scssta.
        when 11.
          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = wa_planilha-value
            importing
              output = wa_nfe-tpemis.
      ENDCASE.

      AT END OF row.
        APPEND wa_nfe TO t_nfe.
      ENDAT.

    ENDLOOP.

  ENDIF.

  LOOP AT t_nfe INTO wa_nfe.

    INSERT INTO znfeinfo VALUES wa_nfe.

    UPDATE j_1bnfdoc
       SET nfenum  = wa_nfe-nfenum
           docstat = wa_nfe-docstat
     WHERE docnum = wa_nfe-docnum.

    UPDATE j_1bnfe_active
       SET docsta = wa_nfe-docstat
           code   = wa_nfe-code
           nfnum9 = wa_nfe-nfenum
           cdv    = wa_nfe-cdv
           scssta = wa_nfe-scssta
     WHERE docnum = wa_nfe-docnum.

    SELECT SINGLE nfenum INTO v_nfenum
      FROM j_1bnfdoc
     WHERE docnum = wa_nfe-docnum.

    IF sy-subrc NE 0.
      t_log2-obj_key        = wa_nfe-obj_key.
      t_log2-interface      = 5.
      t_log2-dt_atualizacao = sy-datum.
      t_log2-hr_atualizacao = sy-uzeit.
      t_log2-type           = 'E'.
      t_log2-id             = 'DADOS NFE'.
      t_log2-num            = 0.
      CONCATENATE 'NFe numero' wa_nfe-nfenum 'com documento' wa_nfe-docnum 'nao atualizado!' INTO t_log2-message SEPARATED BY space.
      t_log2-message_v1     = wa_nfe-docnum.
      t_log2-message_v2     = v_nfenum.
    ELSEIF ( sy-subrc EQ 0 ) AND ( v_nfenum = wa_nfe-nfenum ).
      t_log2-obj_key        = wa_nfe-obj_key.
      t_log2-interface      = 5.
      t_log2-dt_atualizacao = sy-datum.
      t_log2-hr_atualizacao = sy-uzeit.
      t_log2-type           = 'S'.
      t_log2-id             = 'DADOS NFE'.
      t_log2-num            = 0.
      CONCATENATE 'NFe numero' wa_nfe-nfenum 'com documento' wa_nfe-docnum 'atualizado!' INTO t_log2-message SEPARATED BY space.
      t_log2-message_v1     = wa_nfe-docnum.
      t_log2-message_v2     = v_nfenum.
    ENDIF.
    APPEND t_log2.

  ENDLOOP.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_RETURN'
*    TABLES
*      outreturn = t_log2.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = t_log2.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = t_log2.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
  COMMIT WORK.

  MESSAGE 'Notas atualizadas!' TYPE 'I'.
