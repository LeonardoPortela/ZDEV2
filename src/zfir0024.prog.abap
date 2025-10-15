*&--------------------------------------------------------------------&*
*& Report Name    : LOG ALTERAÇÕES EM PARÂMETROS FISCAIS              *&
*& Author         : Grupo André Maggi                                 *&
*& Date           : 18.02.2013                                        *&
*& Funcional Area : Fiscal                                            *&
*&--------------------------------------------------------------------&*
REPORT  zfir0024.

*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: dbtablog,
        zfit0034.
*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_dbtablog,
          logdate  TYPE dbtablog-logdate ,
          logtime  TYPE dbtablog-logtime ,
          logid    TYPE dbtablog-logid   ,
          tabname  TYPE dbtablog-tabname ,
          logkey   TYPE dbtablog-logkey  ,
          hostname TYPE dbtablog-hostname,
          username TYPE dbtablog-username,
          tcode    TYPE dbtablog-tcode   ,
          progname TYPE dbtablog-progname,
          optype   TYPE dbtablog-optype  ,
          versno   TYPE dbtablog-versno  ,
          language TYPE dbtablog-language,
          dataln   TYPE dbtablog-dataln  ,
          logdata  TYPE dbtablog-logdata ,
         END OF ty_dbtablog,

       BEGIN OF ty_dd02t,
         tabname    TYPE dd02t-tabname,
         ddtext     TYPE dd02t-ddtext,
         ddlanguage TYPE dd02t-ddlanguage,
       END OF ty_dd02t,

       BEGIN OF ty_zfit0034,
        logkey      TYPE zfit0034-logkey,
        data        TYPE zfit0034-data,
        hora        TYPE zfit0034-hora,
        usuario     TYPE zfit0034-usuario,
        transacao   TYPE zfit0034-transacao,
        tabela      TYPE zfit0034-tabela,
        desc_tabela TYPE zfit0034-desc_tabela,
        tipo_modif  TYPE zfit0034-tipo_modif,
        status      TYPE zfit0034-status,
        usr_aprov   TYPE zfit0034-usr_aprov,
        dta_aprov   TYPE zfit0034-dta_aprov,
        hr_aprov    TYPE zfit0034-hr_aprov,
       END OF ty_zfit0034,

       BEGIN OF ty_saida,
        data        TYPE zfit0034-data,
        hora        TYPE zfit0034-hora,
        usuario     TYPE zfit0034-usuario,
        transacao   TYPE zfit0034-transacao,
        tabela      TYPE zfit0034-tabela,
        desc_tabela TYPE zfit0034-desc_tabela,
        tipo_modif  TYPE c LENGTH 15,
        status      TYPE zfit0034-status,
        usr_aprov   TYPE zfit0034-usr_aprov,
        dta_aprov   TYPE zfit0034-dta_aprov,
        hr_aprov    TYPE zfit0034-hr_aprov,
        logkey      TYPE zfit0034-logkey,
        visual      TYPE c LENGTH 4,

       END OF ty_saida.

*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
DATA: cl_custom TYPE REF TO cl_gui_custom_container,
      cl_grid   TYPE REF TO cl_gui_alv_grid,

      wa_layout       TYPE lvc_s_layo,
      gs_variant_c    TYPE disvariant,
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.

*&--------------------------------------------------------------------&*
*& TABLE INTERNAL
*&--------------------------------------------------------------------&*
DATA: it_dbtablog TYPE TABLE OF ty_dbtablog,
      it_dd02t    TYPE TABLE OF ty_dd02t,
      it_zfit0034 TYPE TABLE OF ty_zfit0034,
      it_saida    TYPE TABLE OF ty_saida.
*&--------------------------------------------------------------------&*
*& WORK AREA
*&--------------------------------------------------------------------&*
DATA: wa_dbtablog     TYPE ty_dbtablog,
      wa_dd02t        TYPE ty_dd02t,
      wa_zfit0034     TYPE ty_zfit0034,
      wa_saida        TYPE ty_saida,
      insr_zfit0034   TYPE zfit0034.
*&--------------------------------------------------------------------&*
*& SELECTION OPTION
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_logdt FOR dbtablog-logdate,
                p_tcode FOR dbtablog-tcode NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.

  p_tcode-sign   = 'I'.
  p_tcode-option = 'EQ'.
  p_tcode-low    = 'J1BTAX'.
  APPEND p_tcode.
  p_tcode-sign   = 'I'.
  p_tcode-option = 'EQ'.
  p_tcode-low    = 'ZSDT0011'.
  APPEND p_tcode.
  p_tcode-sign   = 'I'.
  p_tcode-option = 'EQ'.
  p_tcode-low    = 'VK11'.
  APPEND p_tcode.
  p_tcode-sign   = 'I'.
  p_tcode-option = 'EQ'.
  p_tcode-low    = 'VK12'.
  APPEND p_tcode.

START-OF-SELECTION.

  PERFORM: gravar_log,
           gerar_report,
           gerar_alv.

  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click


ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.

  CLEAR: wa_saida.

  READ TABLE it_saida INDEX i_row_id INTO wa_saida.

  CASE i_column_id.

    WHEN: 'STATUS'.

      IF NOT ( it_saida[] IS INITIAL ).
        IF ( wa_saida-status EQ 'X' ).
          MESSAGE s899(fi) WITH 'Registro já conferido.'.
        ELSE.
          PERFORM: atualizar_log USING wa_saida.
        ENDIF.

      ENDIF.

    WHEN: 'VISUAL'.
      PERFORM: chamar_programas USING wa_saida.

  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_LOG
*&---------------------------------------------------------------------*
FORM atualizar_log  USING    p_wa_saida STRUCTURE wa_saida.

  IF NOT ( p_wa_saida IS INITIAL ).

    UPDATE zfit0034
      SET
          status     = 'X'
          usr_aprov  = sy-uname
          dta_aprov  = sy-datum
          hr_aprov   = sy-uzeit
     WHERE data      EQ p_wa_saida-data
       AND hora      EQ p_wa_saida-hora
       AND usuario   EQ p_wa_saida-usuario
       AND logkey    EQ p_wa_saida-logkey.


    IF ( sy-subrc EQ 0 ).

      REFRESH: it_saida[].
      CLEAR: wa_saida.

      PERFORM: gerar_report.

      IF NOT ( it_saida[] IS INITIAL ).
        CALL METHOD cl_grid->refresh_table_display.
        MESSAGE s899(fi) WITH 'Registro conferido'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ATUALIZAR_LOG


*&---------------------------------------------------------------------*
*&      Form  GRAVAR_LOG
*&---------------------------------------------------------------------*
FORM gravar_log .




  SELECT logdate logtime logid tabname logkey hostname username tcode progname optype versno language dataln logdata
    FROM dbtablog
    INTO TABLE it_dbtablog
  WHERE logdate  IN p_logdt
    AND tcode    IN p_tcode
    AND language EQ 'PT'.


  CHECK NOT it_dbtablog[] IS INITIAL.

  CLEAR: wa_dbtablog.

  LOOP AT it_dbtablog INTO wa_dbtablog.

    IF ( wa_dbtablog-tcode EQ 'J1BTAX' ) AND ( wa_dbtablog-tabname EQ 'KONP' ).
      DELETE it_dbtablog WHERE logkey  EQ  wa_dbtablog-logkey
                           AND logdate EQ  wa_dbtablog-logdate
                           AND logtime EQ  wa_dbtablog-logtime.
    ENDIF.
    CLEAR: wa_dbtablog.
  ENDLOOP.

  SELECT tabname ddtext ddlanguage
    FROM dd02t
    INTO TABLE it_dd02t
    FOR ALL ENTRIES IN it_dbtablog
  WHERE tabname    EQ it_dbtablog-tabname
    AND ddlanguage EQ 'PT'.


  LOOP AT it_dbtablog INTO wa_dbtablog.

    insr_zfit0034-data        = wa_dbtablog-logdate.
    insr_zfit0034-hora        = wa_dbtablog-logtime.
    insr_zfit0034-usuario     = wa_dbtablog-username.
    insr_zfit0034-transacao   = wa_dbtablog-tcode.
    insr_zfit0034-tabela      = wa_dbtablog-tabname.
    insr_zfit0034-logkey      = wa_dbtablog-logkey.

    READ TABLE it_dd02t INTO wa_dd02t WITH KEY tabname = wa_dbtablog-tabname.
    insr_zfit0034-desc_tabela = wa_dd02t-ddtext.
    insr_zfit0034-tipo_modif  = wa_dbtablog-optype.


    INSERT  INTO zfit0034 VALUES insr_zfit0034.
  ENDLOOP.


ENDFORM.                    " GRAVAR_LOG
*&---------------------------------------------------------------------*
*&      Form  GERAR_REPORT
*&---------------------------------------------------------------------*
FORM gerar_report.

  REFRESH: it_zfit0034[], it_saida[].
  CLEAR: wa_zfit0034, wa_saida.

  SELECT logkey data hora usuario transacao tabela desc_tabela tipo_modif
         status usr_aprov dta_aprov hr_aprov
    FROM zfit0034
    INTO TABLE it_zfit0034
  WHERE data      IN p_logdt
    AND transacao IN p_tcode.

  IF ( it_zfit0034[] IS INITIAL ).
    MESSAGE e899(fi) DISPLAY LIKE 'W' WITH 'Nenhum registro encontrado!'.
  ELSE.


    LOOP AT it_zfit0034 INTO wa_zfit0034.

      wa_saida-data        = wa_zfit0034-data.
      wa_saida-hora        = wa_zfit0034-hora.
      wa_saida-usuario     = wa_zfit0034-usuario.
      wa_saida-transacao   = wa_zfit0034-transacao.
      wa_saida-tabela      = wa_zfit0034-tabela.
      wa_saida-desc_tabela = wa_zfit0034-desc_tabela.


      CASE wa_zfit0034-tipo_modif.
        WHEN: 'I'.
          wa_saida-tipo_modif  = 'INSERIR'.
        WHEN: 'U'.
          wa_saida-tipo_modif  = 'ATUALIZAR'.
        WHEN: 'D'.
          wa_saida-tipo_modif  = 'APAGAR'.
      ENDCASE.


      wa_saida-status      = wa_zfit0034-status.
      wa_saida-usr_aprov   = wa_zfit0034-usr_aprov.
      wa_saida-dta_aprov   = wa_zfit0034-dta_aprov.
      wa_saida-hr_aprov    = wa_zfit0034-hr_aprov.
      wa_saida-visual      = icon_select_detail.
      wa_saida-logkey      = wa_zfit0034-logkey.

      APPEND wa_saida TO it_saida.

      CLEAR: wa_zfit0034, wa_saida.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " GERAR_REPORT
*&---------------------------------------------------------------------*
*&      Form  CHAMAR_PROGRAMAS
*&---------------------------------------------------------------------*
FORM chamar_programas USING p_wa_saida STRUCTURE wa_saida.

  CASE wa_saida-transacao.

    WHEN: 'J1BTAX' OR 'ZSDT0011'.

      SUBMIT rsvtprot
        WITH cusobj EQ wa_saida-tabela
        WITH dbeg   EQ wa_saida-data
        WITH dend   EQ wa_saida-data AND RETURN.

    WHEN: 'VK11' OR 'VK12'.

      SUBMIT rv16achd
        WITH so_date  EQ wa_saida-data
        WITH so_tcode EQ wa_saida-transacao AND RETURN.

  ENDCASE.

ENDFORM.                    " CHAMAR_PROGRAMAS


*&---------------------------------------------------------------------*
*&      Form  GERAR_ALV
*&---------------------------------------------------------------------*
FORM gerar_alv .

  DATA: gr_event_handler TYPE REF TO lcl_event_handler.

  IF ( cl_custom IS INITIAL ).

    CREATE OBJECT cl_custom
      EXPORTING
        container_name              = 'CONTAINER_P'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    PERFORM: display_catalog.

    CREATE OBJECT cl_grid
      EXPORTING
        i_parent          = cl_custom
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_hotspot_click FOR cl_grid.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = gs_variant_c
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fieldcatalog[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.                    " GERAR_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CATALOG
*&---------------------------------------------------------------------*
FORM display_catalog.

  PERFORM fieldcatalog USING:
    'STATUS'       'Status'            '7'   ' ' 'X' 'C500' 'C' ' ' 'X',
    'VISUAL'       'Visualizar'        '10'   '' 'X'  ''    'C' ''  '',
    'DATA'         'Data'              '10'   'X' ' ' ' '    '' ' ' '',
    'HORA'         'Hora'              '10'  ' ' ' ' ' '    ' ' ' ' '',
    'USUARIO'      'Usuário'           '10'  'X' ' ' ' '    ' ' ' ' '',
    'TRANSACAO'    'Transação'         '10'  ' ' ' ' ' '    ' ' ' ' '',
    'TABELA'       'Tabela'            '10'  ' ' ' ' ' '    ' ' ' ' '',
    'DESC_TABELA'  'Descrição'         '20'   ' ' ' ' ' '    ' ' ' ' '',
    'TIPO_MODIF'   'Tipo Modif.'       '10'  'X' ' ' ' '    ' ' ' ' '',
    'USR_APROV'    'Usuário Aprovador' '10'  ' ' ' ' ' '    ' ' ' ' '',
    'DTA_APROV'    'Data Aprovada'     '10'  ' ' ' ' ' ' ' ' 'X' '',
    'HR_APROV'     'Hora Aprovada'     '10'   ' ' ' ' ' '    ' ' ' '  ''.

ENDFORM.                    " DISPLAY_CATALOG

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "pbo OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.

  CASE sy-ucomm.

    WHEN: 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.

    WHEN: 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.

ENDMODULE.                    "pai INPUT
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fieldcatalog  USING    value(p_fieldname)
                            value(p_desc)
                            value(p_tam)
                            value(p_no_zero)
                            value(p_hotspot)
                            value(p_cor)
                            value(p_just)
                            value(p_sum)
                            value(p_check).

  wa_fieldcatalog-fieldname = p_fieldname.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.
  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-no_zero   = p_no_zero.
  wa_fieldcatalog-hotspot   = p_hotspot.
  wa_fieldcatalog-emphasize = p_cor.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-checkbox  = p_check.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

  CLEAR: wa_fieldcatalog.

ENDFORM.                    " FIELDCATALOG
