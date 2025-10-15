************************************************************************
* Program        : ZOPENSIS_CARGA_001                                  *
* Transaction    : ZOPENSIS_006                                        *
* Title          : Carga dos Riscos Compensatórios                     *
* Developer      : Fernando Oliveira                                   *
* Date           : 03/07/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     03/07/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************

REPORT  zopensis_carga_001.

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_line,
         line(100) TYPE c,
       END OF ty_line.

TYPES: BEGIN OF ty_riscos_tcode,
         zcod_risco TYPE ztopensis_002-zcod_risco,
         zcod_tcode TYPE ztopensis_002-zcod_tcode,
         ttext      TYPE tstct-ttext,
       END OF ty_riscos_tcode.

*----------------------------------------------------------------------*
* Declaração de tabelas interna                                        *
*----------------------------------------------------------------------*
DATA: gt_file         TYPE TABLE OF ty_line,
      gt_riscos       TYPE TABLE OF ztopensis_002,
      gt_riscos_tcode TYPE TABLE OF ty_riscos_tcode,
      gt_riscos_desc  TYPE TABLE OF ztopensis_003,
      gt_tstct        TYPE TABLE OF tstct.

DATA gt_sd_opcoes_procto TYPE spopli OCCURS 5 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração de Work-áreas                                             *
*----------------------------------------------------------------------*
DATA: wa_line         LIKE LINE OF gt_file,
      wa_riscos       TYPE ztopensis_002,
      wa_riscos_tcode TYPE ty_riscos_tcode,
      wa_riscos_desc  TYPE ztopensis_003,
      wa_tstct        TYPE tstct.

*----------------------------------------------------------------------*
* Declaração de Variáveis                                              *
*----------------------------------------------------------------------*
DATA: v_filename TYPE string,
      gt_filebin TYPE filetable,
      gs_filestr TYPE file_table,
      v_rc       TYPE i,
      gc_erro.

*----------------------------------------------------------------------*
* Parãmetros de Seleção                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
PARAMETERS: p_file_1  TYPE string.
SELECTION-SCREEN END   OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
SELECTION-SCREEN COMMENT /1(50) comm1.
SELECTION-SCREEN COMMENT /1(50) comm2.
SELECTION-SCREEN COMMENT /1(50) comm3.
SELECTION-SCREEN COMMENT /1(50) comm4.
SELECTION-SCREEN COMMENT /1(50) comm5.
SELECTION-SCREEN COMMENT /1(50) comm6.
SELECTION-SCREEN END   OF BLOCK a2.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-004.
PARAMETERS: p_file_2  TYPE string.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK d1 WITH FRAME TITLE text-006.
SELECTION-SCREEN COMMENT /1(50) comm10.
SELECTION-SCREEN COMMENT /1(50) comm20.
SELECTION-SCREEN COMMENT /1(50) comm30.
SELECTION-SCREEN COMMENT /1(50) comm40.
SELECTION-SCREEN COMMENT /1(50) comm50.
SELECTION-SCREEN COMMENT /1(50) comm60.
SELECTION-SCREEN END   OF BLOCK d1.

SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE text-005.
PARAMETERS: p_sprsl TYPE sprsl MATCHCODE OBJECT bbp_language_value.
PARAMETERS: p_cabec TYPE flag DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK c1.


SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE text-003.
PARAMETERS: p_exib_1 RADIOBUTTON GROUP g1 DEFAULT 'X'.
PARAMETERS: p_exib_2 RADIOBUTTON GROUP g1.
PARAMETERS: p_save   RADIOBUTTON GROUP g1.
PARAMETERS: p_habi   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK a3.

*----------------------------------------------------------------------*
* AT Selection Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file_1.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = gt_filebin
      rc         = v_rc.

  IF sy-subrc IS INITIAL.
    READ TABLE gt_filebin INTO gs_filestr INDEX 1.
    p_file_1 = gs_filestr-filename.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file_2.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table = gt_filebin
      rc         = v_rc.

  IF sy-subrc IS INITIAL.
    READ TABLE gt_filebin INTO gs_filestr INDEX 1.
    p_file_2 = gs_filestr-filename.
  ENDIF.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

  comm2  = 'A Extensão do Arquivo deve ser .CSV'.
  comm4  = 'Coluna 1: Risco'.
  comm5  = 'Coluna 2: Transação'.

  comm20 = 'A Extensão do Arquivo deve ser .CSV'.
  comm40 = 'Coluna 1: Risco'.
  comm50 = 'Coluna 2: Nivel' .
  comm60 = 'Coluna 3: Descrição:'.

START-OF-SELECTION.

  IF p_exib_1 IS NOT INITIAL OR
     p_exib_2 IS NOT INITIAL OR
     p_save   IS NOT INITIAL.
    IF p_sprsl   IS INITIAL OR
       p_file_1  IS INITIAL OR
       p_file_2  IS INITIAL .
      IF p_file_1 IS INITIAL.
        MESSAGE 'Parametro Caminho do Arquivo obrigatório para essa seleção' TYPE 'E'.
      ENDIF.
      IF p_file_2 IS INITIAL.
        MESSAGE 'Parametro Caminho do Arquivo obrigatório para essa seleção' TYPE 'E'.
      ENDIF.
      IF p_sprsl IS INITIAL.
        MESSAGE 'Parametro Idioma obrigatório para essa seleção' TYPE 'E'.
      ENDIF.
    ELSE.
      PERFORM zf_upload_arquivo USING p_file_1.
      PERFORM zf_tratar_dados_risco.
      PERFORM zf_upload_arquivo USING p_file_2.
      PERFORM zf_tratar_dados_risco_nivel.

      CHECK gc_erro IS INITIAL.

      IF p_exib_1 IS NOT INITIAL.
        PERFORM zf_exibir_relatorio_risco.
      ENDIF.
      IF p_exib_2 IS NOT INITIAL.
        PERFORM zf_exibir_relatorio_risco_desc.
      ENDIF.
      IF p_save IS NOT INITIAL.
        PERFORM zf_gravar_dados.
      ENDIF.
    ENDIF.
  ELSEIF p_habi IS NOT INITIAL.
    PERFORM zf_habilitar_riscos.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  ZF_UPLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_upload_arquivo USING p_file.

  v_filename = p_file.

  FREE gt_file[].

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = v_filename
      filetype                = 'ASC'
    TABLES
      data_tab                = gt_file
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF p_cabec IS NOT INITIAL.
      DELETE gt_file INDEX 1.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_RISCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_tratar_dados_risco .

  DATA: li_index TYPE i.

  LOOP AT gt_file INTO wa_line.
    SPLIT wa_line AT ';' INTO wa_riscos-zcod_risco
                              wa_riscos-zcod_tcode.
    CHECK wa_riscos-zcod_risco IS NOT INITIAL.
    CHECK wa_riscos-zcod_tcode IS NOT INITIAL.
    APPEND wa_riscos TO gt_riscos.
  ENDLOOP.

  SELECT *
    FROM tstct
    INTO TABLE gt_tstct
    FOR ALL ENTRIES IN gt_riscos
    WHERE sprsl = p_sprsl AND
          tcode = gt_riscos-zcod_tcode.

  LOOP AT gt_riscos INTO wa_riscos.

    CLEAR: wa_tstct, wa_riscos_tcode.
    READ TABLE gt_tstct INTO wa_tstct WITH KEY tcode = wa_riscos-zcod_tcode.

    wa_riscos_tcode-zcod_risco = wa_riscos-zcod_risco.
    wa_riscos_tcode-zcod_tcode = wa_riscos-zcod_tcode.
    wa_riscos_tcode-ttext      = wa_tstct-ttext.

    APPEND wa_riscos_tcode TO gt_riscos_tcode.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATAR_DADOS_RISCO_NIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_tratar_dados_risco_nivel .

  DATA: li_index TYPE i.

  LOOP AT gt_file INTO wa_line.
    SPLIT wa_line AT ';' INTO wa_riscos_desc-zcod_risco
                              wa_riscos_desc-zcod_nivel_risco
                              wa_riscos_desc-zdsc_risco.
    wa_riscos_desc-zcod_idioma = p_sprsl.

    CASE wa_riscos_desc-zcod_nivel_risco.
      WHEN 'A'.
        wa_riscos_desc-zcod_nivel_risco = 3.
      WHEN 'M'.
        wa_riscos_desc-zcod_nivel_risco = 2.
      WHEN 'B'.
        wa_riscos_desc-zcod_nivel_risco = 1.
      WHEN 1 OR 2 OR 3.
      WHEN OTHERS.
        gc_erro = 'X'.
    ENDCASE.

    APPEND wa_riscos_desc TO gt_riscos_desc.
  ENDLOOP.

  IF gc_erro IS NOT INITIAL.
    MESSAGE 'Erro no nível do Risco' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_exibir_relatorio_risco.

  DATA: r_table     TYPE REF TO cl_salv_table,
        r_functions TYPE REF TO cl_salv_functions.

  TRY .
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = r_table
        CHANGING
          t_table      = gt_riscos_tcode.
    CATCH cx_salv_msg.
  ENDTRY.

  CALL METHOD r_table->display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EXIBIR_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_exibir_relatorio_risco_desc.

  DATA: r_table     TYPE REF TO cl_salv_table,
        r_functions TYPE REF TO cl_salv_functions.

  TRY .
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = r_table
        CHANGING
          t_table      = gt_riscos_desc.
    CATCH cx_salv_msg.
  ENDTRY.

  CALL METHOD r_table->display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_gravar_dados .

  MODIFY ztopensis_002 FROM TABLE gt_riscos.
  COMMIT WORK.

  MODIFY ztopensis_003 FROM TABLE gt_riscos_desc.
  COMMIT WORK.

  MESSAGE 'Carga Efetuada com Sucesso' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HABILITAR_RISCOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_habilitar_riscos .

  DATA: lt_ztopensis_003   TYPE TABLE OF ztopensis_003.
  DATA: ls_ztopensis_003   TYPE ztopensis_003.

  DATA: vl_resposta TYPE c,
        vl_ajuste.

  SELECT *
    FROM ztopensis_003
    INTO TABLE lt_ztopensis_003.

  IF sy-subrc <> 0.
    MESSAGE 'Não existe Risco na Base para ser Habilitado' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT lt_ztopensis_003 INTO ls_ztopensis_003.
    IF ls_ztopensis_003-zflg_risco_habil = 'X'.
      gt_sd_opcoes_procto-selflag   = 'X'.
    ENDIF.

    CONCATENATE ls_ztopensis_003-zcod_risco '-'
                ls_ztopensis_003-zdsc_risco
                INTO gt_sd_opcoes_procto-varoption SEPARATED BY space.
    APPEND gt_sd_opcoes_procto.
    CLEAR  gt_sd_opcoes_procto.
  ENDLOOP.

  CLEAR: vl_resposta, vl_ajuste.

  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      mark_flag          = 'X'
      mark_max           = 0
      start_col          = 15
      start_row          = 3
      textline1          = 'Selecione o Risco desejado:'
      titel              = 'Controle Compensatório'
    IMPORTING
      answer             = vl_resposta
    TABLES
      t_spopli           = gt_sd_opcoes_procto
    EXCEPTIONS
      not_enough_answers = 1
      too_much_answers   = 2
      too_much_marks     = 3
      OTHERS             = 4.

  IF vl_resposta = space.

    IF gt_sd_opcoes_procto[] IS NOT INITIAL.
      LOOP AT lt_ztopensis_003 INTO ls_ztopensis_003.
        READ TABLE gt_sd_opcoes_procto WITH KEY varoption(04) = ls_ztopensis_003-zcod_risco.
        CHECK sy-subrc = 0.

        IF gt_sd_opcoes_procto-selflag <> ls_ztopensis_003-zflg_risco_habil.
          vl_ajuste = 'X'.
        ENDIF.

        IF gt_sd_opcoes_procto-selflag = 'X'.
          ls_ztopensis_003-zflg_risco_habil = 'X'.
        ELSE.
          ls_ztopensis_003-zflg_risco_habil = space.
        ENDIF.
        MODIFY lt_ztopensis_003 FROM ls_ztopensis_003.
      ENDLOOP.
      MODIFY ztopensis_003 FROM TABLE lt_ztopensis_003.
      COMMIT WORK.
    ENDIF.

    IF vl_ajuste IS NOT INITIAL.
      MESSAGE 'Ajuste realizado com sucesso' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.
