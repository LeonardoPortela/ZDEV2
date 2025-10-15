************************************************************************
*              ******************************************              *
*              *                 AMAGGI                 *              *
*              *       CONFIDENCIAL E PROPRIETÁRIO      *              *
*              *      TODOS OS DIREITOS RESERVADOS      *              *
*              ******************************************              *
************************************************************************
* Projeto       : Projeto Controle Ferramentaria                       *
* Objetivo      : Carga Inicial de Materiais                           *
* Analista      : Alexandre Suzan WAGON                                *
* Desenvolvedor : Alexandre Suzan WAGON                                *
* Data          : 24/11/2020                                           *
* Transação     : ZPMR0065                                             *
* Observação    : Arquivo para upload deve ser salvo como              *
*                 Texto(tab delimited)(*.txt)                          *
*----------------------------------------------------------------------*
REPORT zpmr0065 MESSAGE-ID z_mm NO STANDARD PAGE HEADING.

TABLES: sscrfields.

*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_reg,
         w_conteudo TYPE max_segm,
       END   OF ty_reg,

       BEGIN OF yg_alv,
         werks        TYPE werks_d,                       " Centro
         matnr        TYPE matnr,                         " Material
         equnr        TYPE equnr,                         "EQUIPAMENTO
         labst        TYPE c LENGTH 16,                   " Quantidade
         lgort        TYPE lgort_d,                       " Depósito
         bldat        TYPE bldat,
         lifnr        TYPE lifnr,
         ref_doc      TYPE c LENGTH 15,                   " NF Material
         txt_cab      TYPE c LENGTH 70,                   " Texto Cabeçalho
         tp_mov       TYPE c LENGTH 03,                   " Tipo de Movimento
         meins        TYPE meins,                         " Unidade de Medida
         proces       TYPE c LENGTH 01,                   " Processado
         obs(100)     TYPE c,
         descr(50)    TYPE c,
         descr_eq(50) TYPE c,
         arquivo      TYPE zpmt0039-arquivo,
         celltab      TYPE lvc_t_styl,
       END   OF yg_alv,


       BEGIN OF ty_log,
         id       TYPE symsgid,                       " Semaforo
         werks    TYPE werks_d,                       " Centro
         matnr    TYPE matnr,                         " Material
         equnr    TYPE equnr,                         " Equipamento
         eqktx    TYPE char50,                         " Descrição equipamento
         meins    TYPE meins,                         " Unidade de Medida
         labst    TYPE labst,                         " Quantidade
         lgort    TYPE lgort_d,                       " Depósito
         bldat    TYPE bldat,
         lifnr    TYPE lifnr,
         ref_doc  TYPE c LENGTH 15,                   " NF Material
         txt_cab  TYPE c LENGTH 70,                   " Texto Cabeçalho
         tp_mov   TYPE c LENGTH 03,                   " Tipo de Movimento
         mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,  " Documento Material
         doc_year TYPE bapi2017_gm_head_ret-doc_year, " Documento Ano
         observ   TYPE c LENGTH 220,                  " Observação
         arquivo  TYPE zpmt0039-arquivo,
         type     TYPE zpmt0039-type,
       END   OF ty_log.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
DATA: tg_alv_arq  TYPE TABLE OF yg_alv,
      tg_alv      TYPE TABLE OF yg_alv,
      tg_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat,
      tg_log      TYPE TABLE OF ty_log.

*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
DATA: eg_layout     TYPE lvc_s_layo.
DATA: eg_alv             TYPE yg_alv,
      eg_log             TYPE ty_log,
      gwa_datageneral    LIKE bapi_itob,
      gva_equipment      TYPE bapi_itob_parms-equipment,
      gwa_data_generalx  TYPE bapi_itobx,
      gwa_dataspecific   LIKE bapi_itob_eq_only,
      gwa_data_specificx TYPE bapi_itob_eq_onlyx,
      gwa_return         LIKE bapiret2,
      git_xtensionin     TYPE TABLE OF bapiparex.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: vg_file TYPE string,
      vg_code TYPE sy-ucomm.

DATA: icon_proc TYPE string.

*** Anexos:
DATA: t_anexos     TYPE TABLE OF bdn_con,
      l_obj_key    TYPE sibflporb-instid,
      l_lines      TYPE i,
      anexo_obj    TYPE REF TO cl_gos_manager,
      l_ip_mode    TYPE sgs_rwmod,
      l_ip_service TYPE sgs_srvnam,
      w_bor        TYPE borident.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  cg_erro_id TYPE c LENGTH 08 VALUE '@S_TL_R@',
  cg_suce_id TYPE c LENGTH 08 VALUE '@S_TL_G@'.

*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: og_alv   TYPE REF TO cl_gui_alv_grid,
      o_cont   TYPE REF TO cl_gui_docking_container,
      o_parent TYPE REF TO cl_gui_container.

*&---------------------------------------------------------------------*
*&      CLASS: LCL_EVENT DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.                        "#EC *
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object," e_interactive,
      handle_menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING e_object e_ucomm,
      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_event DEFINITION
*&---------------------------------------------------------------------*
*&      CLASS: lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    PERFORM zf_handle_toolbar USING e_object.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button.
    PERFORM zf_handle_menu_button USING e_ucomm
                                        e_object.
  ENDMETHOD.                    "handle_menu_button

  METHOD handle_user_command.
    PERFORM zf_handle_user_command USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_event IMPLEMENTATION
DATA v_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: pc_arq_p TYPE rlgrap-filename,
              pc_budat TYPE mkpf-budat OBLIGATORY,
              pc_trans AS CHECKBOX DEFAULT ' '. "04/09/2025 - ggaraujo1 - IR250119 - Melhoria Proc. Gerenciamento de Ferramentas
  SELECTION-SCREEN SKIP.

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.

  icon_proc = icon_attachment && 'DOWNLOAD PLANILHA PADRÃO CARGA'.
  sscrfields-functxt_01 = icon_proc .

AT SELECTION-SCREEN. "PAI
  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.
      PERFORM f_importar_anexos.
  ENDCASE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pc_arq_p.
  PERFORM zf_busca_arquivo.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM: zf_verifica_arquivo,
           zf_upload_arquivo,
           zf_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_ARQUIVO
*&---------------------------------------------------------------------*
FORM zf_busca_arquivo.

  DATA: tl_file  TYPE filetable.

  DATA: el_file  TYPE file_table.

  DATA: vl_subrc TYPE i,
        vl_title TYPE string_unicode.

  vl_title = 'Seleciona arquivo de carga'(t13).

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_title
      file_filter             = '*.xls*|*.XLS*'
    CHANGING
      file_table              = tl_file
      rc                      = vl_subrc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE 'Erro ao selecionar Arquivo'(006) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  READ TABLE tl_file INTO el_file INDEX 1.
  IF sy-subrc IS INITIAL.
    pc_arq_p = el_file-filename.
  ENDIF.

  CLEAR: el_file,
         vl_subrc,
         vl_title.

  FREE tl_file.

ENDFORM.                    " ZF_BUSCA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  zf_verifica_arquivo
*&---------------------------------------------------------------------*
FORM zf_verifica_arquivo.

  DATA vl_result(1) TYPE c.

  vg_file = pc_arq_p.

  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = vg_file
    RECEIVING
      result               = vl_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

  IF NOT sy-subrc IS INITIAL
  OR vl_result EQ space.
    MESSAGE s000(z_mm) DISPLAY LIKE 'E' WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " zf_verifica_arquivo
*&---------------------------------------------------------------------*
*&      Form  zf_upload_arquivo
*&---------------------------------------------------------------------*
FORM zf_upload_arquivo.

  DATA: tl_reg  TYPE TABLE OF ty_reg,
        lv_erro TYPE c.

  DATA: el_reg TYPE ty_reg,
        el_alv TYPE yg_alv.

  FIELD-SYMBOLS:
    <fl_alv>     TYPE yg_alv,
    <fl_alv_arq> TYPE yg_alv.

  DATA: vl_labst TYPE c LENGTH 16,
        vl_verpr TYPE c LENGTH 13,
        vl_total TYPE c LENGTH 27,
        vl_char  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

  DATA: vl_filename TYPE rlgrap-filename.
  DATA: tl_intern  TYPE TABLE OF alsmex_tabline.
  DATA: el_intern  TYPE alsmex_tabline.

  DATA: lv_stripped_name TYPE string,
        lv_file_path     TYPE string.

  REFRESH: tg_alv,
           tg_alv_arq.

  vl_filename = vg_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = vl_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 8
      i_end_row               = 999999
    TABLES
      intern                  = tl_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.


  READ TABLE tl_intern INTO el_intern  INDEX 1.

  DATA: gd_currentrow TYPE sy-tabix.

  gd_currentrow = el_intern-row.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = vl_filename
    IMPORTING
      stripped_name = lv_stripped_name
      file_path     = lv_file_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  LOOP AT tl_intern INTO el_intern.

    IF el_intern-row NE gd_currentrow.

      SHIFT eg_alv-matnr LEFT DELETING LEADING '0'.
      CONDENSE eg_alv-matnr NO-GAPS.

      FIND ',' IN eg_alv-labst.
      IF sy-subrc EQ 0.
        PERFORM zf_trata_numero USING eg_alv-labst
                                      eg_alv-labst
                                      '3'.
      ENDIF.

      UNASSIGN <fl_alv_arq>.
      APPEND INITIAL LINE TO tg_alv_arq ASSIGNING <fl_alv_arq>.
      MOVE: eg_alv-matnr    TO <fl_alv_arq>-matnr   ,
            eg_alv-werks    TO <fl_alv_arq>-werks   ,
            eg_alv-equnr    TO <fl_alv_arq>-equnr   ,
            eg_alv-lgort    TO <fl_alv_arq>-lgort   ,
*           eg_alv-labst    TO <fl_alv_arq>-labst   ,
            1               TO <fl_alv_arq>-labst   ,
            eg_alv-tp_mov   TO <fl_alv_arq>-tp_mov  ,
            eg_alv-txt_cab  TO <fl_alv_arq>-txt_cab ,
            eg_alv-bldat    TO <fl_alv_arq>-bldat   ,
            eg_alv-lifnr    TO <fl_alv_arq>-lifnr   ,
            eg_alv-ref_doc  TO <fl_alv_arq>-ref_doc .

      CONDENSE: <fl_alv_arq>-labst NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = eg_alv-matnr
        IMPORTING
          output = eg_alv-matnr.

      APPEND eg_alv TO tg_alv.
      CLEAR eg_alv.

      gd_currentrow = el_intern-row.

    ENDIF.

    CASE el_intern-col.

      WHEN '0001'.
        eg_alv-werks = el_intern-value.
        eg_alv-lifnr = el_intern-value.

      WHEN '0002'.
        eg_alv-matnr = el_intern-value.

      WHEN '0003'.
        eg_alv-equnr = el_intern-value.

      WHEN '0004'.
*       eg_alv-labst = el_intern-value.
        eg_alv-labst = 1.

      WHEN '0005'.
        eg_alv-lgort = el_intern-value.

      WHEN '0006'.
        eg_alv-bldat = el_intern-value+6(4) && el_intern-value+3(2) && el_intern-value(2).

      WHEN '0007'.
        eg_alv-ref_doc = el_intern-value.

      WHEN '0008'.
        eg_alv-txt_cab = el_intern-value.

    ENDCASE.

    eg_alv-tp_mov = 'ZFR'.
    eg_alv-arquivo = lv_stripped_name.

  ENDLOOP.

  IF tl_intern[] IS NOT INITIAL.

    SHIFT eg_alv-matnr LEFT DELETING LEADING '0'.
    CONDENSE eg_alv-matnr NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = eg_alv-matnr
      IMPORTING
        output = eg_alv-matnr.

    FIND ',' IN eg_alv-labst.
    IF sy-subrc EQ 0.
      PERFORM zf_trata_numero USING eg_alv-labst
                                    eg_alv-labst
                                    '3'.
    ENDIF.

    UNASSIGN <fl_alv_arq>.
    APPEND INITIAL LINE TO tg_alv_arq ASSIGNING <fl_alv_arq>.
    MOVE: eg_alv-matnr    TO <fl_alv_arq>-matnr   ,
          eg_alv-werks    TO <fl_alv_arq>-werks   ,
          eg_alv-equnr    TO <fl_alv_arq>-equnr   ,
          eg_alv-lgort    TO <fl_alv_arq>-lgort   ,
*         eg_alv-labst    TO <fl_alv_arq>-labst   ,
          1               TO <fl_alv_arq>-labst   ,
          eg_alv-tp_mov   TO <fl_alv_arq>-tp_mov  ,
          eg_alv-txt_cab  TO <fl_alv_arq>-txt_cab ,
          eg_alv-bldat    TO <fl_alv_arq>-bldat   ,
          eg_alv-lifnr    TO <fl_alv_arq>-lifnr   ,
          eg_alv-ref_doc  TO <fl_alv_arq>-ref_doc .

    CONDENSE: <fl_alv_arq>-labst NO-GAPS.

    APPEND eg_alv TO tg_alv.
    CLEAR eg_alv.

  ENDIF.

  DELETE tg_alv WHERE proces EQ abap_true.
  IF tg_alv IS INITIAL.
    MESSAGE s000(z_mm) WITH 'Materiais já processados!'
                 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.

    DATA(lt_alv) = tg_alv.

    DESCRIBE TABLE lt_alv LINES DATA(lv_lines).

    SORT lt_alv BY equnr.
    DELETE ADJACENT DUPLICATES FROM lt_alv COMPARING equnr.

    DESCRIBE TABLE lt_alv LINES DATA(lv_lines1).

    IF lv_lines > lv_lines1.
      MESSAGE 'Existem equipamentos duplicados, carga não realizada' TYPE 'S' DISPLAY LIKE 'E'.
      REFRESH tg_alv.
      EXIT.
    ELSE.

      LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_alv>-equnr
          IMPORTING
            output = <fs_alv>-equnr.

      ENDLOOP.

      SELECT *
        FROM zpmt0039
        INTO TABLE @DATA(lt_0039)
        FOR ALL ENTRIES IN @lt_alv
        WHERE equnr = @lt_alv-equnr AND type NE 'E'.
      IF sy-subrc IS INITIAL AND pc_trans IS INITIAL. "04/09/2025 - ggaraujo1 - IR250119 - Melhoria Proc. Gerenciamento de Ferramentas


        MESSAGE 'Existem equipamentos que já foram processados, carga não realizada!' TYPE 'S' DISPLAY LIKE 'E'.
        REFRESH tg_alv.
        EXIT.

      ELSE.

        SELECT equnr,iwerk
          FROM equz
          INTO TABLE @DATA(lt_equz)
          FOR ALL ENTRIES IN @lt_alv
          WHERE equnr = @lt_alv-equnr.
        IF sy-subrc IS INITIAL.
          SORT lt_equz BY equnr iwerk.

          LOOP AT lt_alv ASSIGNING <fs_alv>.
            READ TABLE lt_equz TRANSPORTING NO FIELDS
            WITH KEY equnr = <fs_alv>-equnr
                     iwerk = <fs_alv>-werks
            BINARY SEARCH.
            IF sy-subrc IS NOT INITIAL.

              CONCATENATE 'Equipamento' <fs_alv>-equnr 'não pertence ao centro' <fs_alv>-werks ',' 'favor validar' INTO DATA(lv_msg) SEPARATED BY space.
              MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
              lv_erro = abap_true.
              EXIT.
            ENDIF.

          ENDLOOP.

          IF lv_erro IS NOT INITIAL.
            REFRESH tg_alv.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  PERFORM zf_estorno. "04/09/2025 - ggaraujo1 - IR250119 - Melhoria Proc. Gerenciamento de Ferramentas
  PERFORM zf_convert.

ENDFORM.                    " zf_upload_arquivo
*&---------------------------------------------------------------------*
*&      Form  ZF_TRATA_NUMERO
*&---------------------------------------------------------------------*
FORM zf_trata_numero USING p_char
                           p_num
                     VALUE(p_dec).

  DATA vl_char(100).

  DATA: vl_int TYPE c LENGTH 10,
        vl_per TYPE c LENGTH 10,
        vl_tam TYPE i.

  SPLIT p_char AT ',' INTO vl_int
                           vl_per.
  CONDENSE: vl_int NO-GAPS,
            vl_per NO-GAPS.

  MOVE strlen( vl_per ) TO vl_tam.
  IF vl_tam NE p_dec.
    CLEAR p_dec.
    MOVE vl_tam TO p_dec.
  ENDIF.

  vl_char = p_char.
  TRANSLATE vl_char USING '. '.
  TRANSLATE vl_char USING ', '.
  CONDENSE vl_char NO-GAPS.
  SHIFT vl_char RIGHT DELETING TRAILING space.
  TRANSLATE vl_char USING ' 0'.
  p_num = vl_char.
  p_num = p_num / ( 10 ** p_dec ).

ENDFORM.                    " trata_numero
*&---------------------------------------------------------------------*
*&      Form  zf_de_para
*&---------------------------------------------------------------------*
FORM zf_convert.

  DATA: tl_alv           TYPE TABLE OF yg_alv.
  DATA: vl_mtart         TYPE mara-mtart.

  DATA: vl_matnr         TYPE matnr.
  DATA vl_equnr           TYPE equnr.

  DATA: lt_zpmt0039 TYPE TABLE OF zpmt0039,
        ls_zpmt0039 TYPE zpmt0039.

  FIELD-SYMBOLS:
        <fl_alv>         TYPE yg_alv.

  DATA: lt_celltab TYPE lvc_t_styl.

  IF tg_alv[] IS NOT INITIAL.
    SELECT * INTO TABLE lt_zpmt0039
      FROM zpmt0039
      FOR ALL ENTRIES IN tg_alv
    WHERE arquivo  = tg_alv-arquivo
      AND werks    = tg_alv-werks
      AND matnr    = tg_alv-matnr
      AND lgort    = tg_alv-lgort
      AND bldat    = tg_alv-bldat
      AND lifnr    = tg_alv-lifnr
      AND ref_doc  = tg_alv-ref_doc.

    SORT lt_zpmt0039 BY arquivo
                        werks
                        matnr
                        meins
                        lgort
                        bldat
                        lifnr
                        ref_doc.

  ENDIF.

  UNASSIGN <fl_alv>.
  LOOP AT tg_alv ASSIGNING <fl_alv>.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fl_alv>-matnr
      IMPORTING
        output = <fl_alv>-matnr.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = <fl_alv>-matnr(18)
      IMPORTING
        output = <fl_alv>-matnr.

** Identifica Unidade de Medida cadastrada
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fl_alv>-matnr
      IMPORTING
        output = vl_matnr.

    SELECT SINGLE meins mtart
      FROM mara
      INTO ( <fl_alv>-meins, vl_mtart )
     WHERE matnr EQ vl_matnr.

    SELECT SINGLE maktx
      FROM makt
      INTO <fl_alv>-descr
     WHERE matnr EQ vl_matnr
       AND spras = sy-langu.


    IF vl_mtart NE 'UNBW'.
      REFRESH lt_celltab.
      PERFORM fill_celltab USING 'RO'
                           CHANGING lt_celltab.
      <fl_alv>-proces = 'X'.
      <fl_alv>-celltab[] = lt_celltab[].
      <fl_alv>-obs = 'Material Não é do tipo UNBW.'.
    ELSE.
*ZPP-CS2022000423 MelhoriasProcFerramentCargaIn-BG #76549 -INICIO


      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_alv>-equnr
        IMPORTING
          output = vl_equnr.



      SELECT SINGLE eqktx
        FROM eqkt
        INTO <fl_alv>-descr_eq
        WHERE equnr EQ vl_equnr.


      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_alv>-equnr
        IMPORTING
          output = <fl_alv>-equnr.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
        EXPORTING
          input  = <fl_alv>-equnr(18)
        IMPORTING
          output = <fl_alv>-equnr.

** Identifica Unidade de Medida cadastrada
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fl_alv>-matnr
        IMPORTING
          output = vl_matnr.

      IF <fl_alv>-equnr IS NOT INITIAL.
        "verifica se o material é do tipo D e grupo de autorização = 0011 #FF Ajustes S4H (Solic. Guilherme F.)
        SELECT SINGLE * FROM equi INTO @DATA(w_equi)
          WHERE equnr EQ @vl_equnr
            AND eqtyp = 'D'
            AND begru = '0011'.

        IF sy-subrc IS NOT INITIAL.
          IF <fl_alv>-proces IS INITIAL.
            REFRESH lt_celltab.
            PERFORM fill_celltab USING 'RO'
                                 CHANGING lt_celltab.
            <fl_alv>-proces = 'X'.
            <fl_alv>-celltab[] = lt_celltab[].
            <fl_alv>-obs = 'Equip diferente ctg D ou grp autoriz diferente de 0011'.
          ENDIF.
        ENDIF.
        "Verifica se existe o equipamento ja possui material vinculado
        SELECT SINGLE * FROM equz INTO @DATA(w_equz)
          WHERE equnr EQ @vl_equnr
          AND datbi = '99991231'.
*
        IF sy-subrc IS INITIAL.

          IF w_equz-submt IS NOT INITIAL.
            IF <fl_alv>-proces IS INITIAL.
              REFRESH lt_celltab.
              PERFORM fill_celltab USING 'RO'
                                   CHANGING lt_celltab.
              <fl_alv>-proces = 'X'.
              <fl_alv>-celltab[] = lt_celltab[].
              <fl_alv>-obs = 'Equipamento já possui material atribuído, favor verificar'.
            ENDIF.
          ENDIF.
          "Verifica se o centro informado é o mesmo da tabela
          IF w_equz-iwerk NE <fl_alv>-werks.
            IF <fl_alv>-proces IS INITIAL.
              REFRESH lt_celltab.
              PERFORM fill_celltab USING 'RO'
                                   CHANGING lt_celltab.
              <fl_alv>-proces = 'X'.
              <fl_alv>-celltab[] = lt_celltab[].
              <fl_alv>-obs = 'Equipamento com centro diferente de Material'.
            ENDIF.
          ENDIF.

        ENDIF.
      ELSE.
        IF <fl_alv>-proces IS INITIAL.
          REFRESH lt_celltab.
          PERFORM fill_celltab USING 'RO'
                               CHANGING lt_celltab.
          <fl_alv>-proces = 'X'.
          <fl_alv>-celltab[] = lt_celltab[].
          <fl_alv>-obs = 'Equipamento não atribuído, favor verificar'.
        ENDIF.
      ENDIF.

*     ZPP-CS2022000423 MelhoriasProcFerramentCargaIn-BG #76549 - Fim

      READ TABLE lt_zpmt0039 INTO ls_zpmt0039  WITH KEY arquivo  = <fl_alv>-arquivo
                                                        werks    = <fl_alv>-werks
                                                        matnr    = vl_matnr
                                                        equnr    = <fl_alv>-equnr
                                                        lgort    = <fl_alv>-lgort
                                                        bldat    = <fl_alv>-bldat
                                                        lifnr    = <fl_alv>-lifnr
                                                        ref_doc  = <fl_alv>-ref_doc BINARY SEARCH.
      IF sy-subrc = 0.
        IF ls_zpmt0039-mat_doc IS NOT INITIAL.
          IF <fl_alv>-proces IS INITIAL.
            CONCATENATE 'Processado anteriormente: ' ls_zpmt0039-mat_doc '-' ls_zpmt0039-doc_year INTO <fl_alv>-obs.

            PERFORM fill_celltab USING 'RO'
                                 CHANGING lt_celltab.
            <fl_alv>-proces = 'X'.
            INSERT LINES OF lt_celltab INTO TABLE <fl_alv>-celltab.
          ENDIF.
        ELSEIF ls_zpmt0039-observ IS NOT INITIAL.
          <fl_alv>-obs = ls_zpmt0039-observ.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  zf_exibe_alv
*&---------------------------------------------------------------------*
FORM zf_exibe_alv.
  CHECK tg_alv IS NOT INITIAL.
  CALL SCREEN 0100.
ENDFORM.                    "zf_exibe_alv
*&---------------------------------------------------------------------*
*&      Form  zf_handle_toolbar
*&---------------------------------------------------------------------*
FORM zf_handle_toolbar USING e_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: el_toolbar TYPE stb_button.

  CLEAR el_toolbar.
  el_toolbar-butn_type  = 3.
  APPEND el_toolbar TO e_object->mt_toolbar.

  CLEAR el_toolbar.
  el_toolbar-function   = 'SELECT'.
  el_toolbar-icon       = icon_select_all.
  el_toolbar-butn_type  = 0.
  el_toolbar-quickinfo  = 'Marcar'(t10).
  el_toolbar-disabled   = space.
  APPEND el_toolbar TO e_object->mt_toolbar.

  CLEAR el_toolbar.
  el_toolbar-function   = 'DESELECT'.
  el_toolbar-icon       = icon_deselect_all.
  el_toolbar-butn_type  = 0.
  el_toolbar-quickinfo  = 'Desmarcar'(t11).
  el_toolbar-disabled   = space.
  APPEND el_toolbar TO e_object->mt_toolbar.

  CLEAR el_toolbar.
  el_toolbar-function   = 'INSERT'.
  el_toolbar-icon       = icon_create.
  el_toolbar-butn_type  = 0.
  el_toolbar-quickinfo = 'Carga Materiais'(t12).
  el_toolbar-text      = 'Carga Materiais'(t12).
  APPEND el_toolbar TO e_object->mt_toolbar.

ENDFORM.                    "zf_handle_toolbar
*&---------------------------------------------------------------------*
*&      Form  zf_handle_menu_button
*&---------------------------------------------------------------------*
FORM zf_handle_menu_button  USING e_ucomm  TYPE        sy-ucomm
                                  e_object TYPE REF TO cl_ctmenu.
  CASE e_ucomm.
    WHEN 'SELECT'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'SELECT'
          text  = 'Select All'(t14).
    WHEN 'DESELECT'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'DESELECT'
          text  = 'De Select All'(t15).
    WHEN 'INSERT'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'INSERT'
          text  = 'Carga Materiais'(t16).
  ENDCASE.
ENDFORM.                    "zf_handle_menu_button
*&---------------------------------------------------------------------*
*&      Form  zf_handle_user_command
*&---------------------------------------------------------------------*
FORM zf_handle_user_command USING x_ucomm.

  DATA: tl_alv    TYPE TABLE OF yg_alv,
        tl_item   TYPE TABLE OF bapi2017_gm_item_create,
        tl_return TYPE TABLE OF bapiret2.

  FIELD-SYMBOLS:
    <fl_alv>  TYPE yg_alv,
    <fl_item> TYPE bapi2017_gm_item_create,
    <fl_log>  TYPE ty_log.

  DATA: el_header TYPE bapi2017_gm_head_01,
        el_code   TYPE bapi2017_gm_code,
        el_return TYPE bapiret2.

  DATA: vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
        vl_doc_year TYPE bapi2017_gm_head_ret-doc_year.

  DATA: lt_celltab TYPE lvc_t_styl.

  CASE x_ucomm.
    WHEN 'SELECT'.
      UNASSIGN <fl_alv>.
      LOOP AT tg_alv ASSIGNING <fl_alv>.
        CHECK <fl_alv>-proces IS INITIAL.
        MOVE abap_true TO <fl_alv>-proces.
      ENDLOOP.

    WHEN 'DESELECT'.
      UNASSIGN <fl_alv>.
      LOOP AT tg_alv ASSIGNING <fl_alv>.
        <fl_alv>-proces = ''.
      ENDLOOP.

    WHEN 'INSERT'.
      REFRESH tl_alv.
      APPEND LINES OF tg_alv TO tl_alv.
      SORT tl_alv BY proces ASCENDING.
      DELETE tl_alv WHERE proces EQ abap_true.
      DELETE tl_alv WHERE obs(10) = 'Processado'.
      DELETE tl_alv WHERE obs(10) = 'PROCESSADO'.
      IF tl_alv IS INITIAL.
        MESSAGE s000(z_mm) WITH 'Verificar coluna Observações'
                          'para o processamento'
                     DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      REFRESH tg_log.
      LOOP AT tg_alv ASSIGNING <fl_alv>.

        IF <fl_alv>-proces = abap_true.
          CONTINUE.
        ELSEIF  <fl_alv>-obs(10) = 'PROCESSADO' OR
            <fl_alv>-obs(10) = 'Processado'.
          CONTINUE.
        ENDIF.

        CLEAR: el_header,
               el_code.
        el_header-pstng_date = pc_budat.
        el_header-doc_date   = <fl_alv>-bldat.
        el_header-pr_uname   = sy-uname.
        el_header-header_txt = <fl_alv>-txt_cab.
        el_code-gm_code      = '05'.

        REFRESH tl_item.
        UNASSIGN <fl_item>.
        APPEND INITIAL LINE TO tl_item ASSIGNING <fl_item>.
*--> 16.06.2023 - Migration S4 – MIGNOW - Start
        "        MOVE: <fl_alv>-matnr  TO <fl_item>-material,
        DATA(v_len4) = strlen( <fl_alv>-matnr ).
        IF v_len4 > 18.
          <fl_item>-material_long = <fl_alv>-matnr .
        ELSE.
          <fl_item>-material      = <fl_alv>-matnr .
        ENDIF.
        MOVE:
*<-- 16.06.2023 - Migration S4 – MIGNOW – End
                      <fl_alv>-werks  TO <fl_item>-plant,
                      <fl_alv>-lgort  TO <fl_item>-stge_loc,
                      <fl_alv>-labst  TO <fl_item>-entry_qnt,
                      <fl_alv>-meins  TO <fl_item>-entry_uom.
**
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <fl_item>-material
          IMPORTING
            output = <fl_item>-material.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = <fl_alv>-equnr
          IMPORTING
            output = <fl_alv>-equnr.

        MOVE <fl_alv>-tp_mov TO <fl_item>-move_type.

        SET UPDATE TASK LOCAL.
        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = el_header
            goodsmvt_code    = el_code
          IMPORTING
            materialdocument = vl_mat_doc
            matdocumentyear  = vl_doc_year
          TABLES
            goodsmvt_item    = tl_item
            return           = tl_return.

        READ TABLE tl_return INTO el_return
                             WITH KEY type = 'E'.
        IF NOT sy-subrc IS INITIAL.
          MOVE abap_true TO <fl_alv>-proces.
          CONCATENATE 'Processado Corretamente: ' vl_mat_doc '-' vl_doc_year INTO <fl_alv>-obs.
          gwa_datageneral-consttype = <fl_item>-material.

          gva_equipment = <fl_alv>-equnr.
          gwa_data_generalx-consttype     = 'X'.
          CALL FUNCTION 'BAPI_EQUI_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              equipment      = gva_equipment
              data_general   = gwa_datageneral
              data_generalx  = gwa_data_generalx
              data_specific  = gwa_dataspecific
              data_specificx = gwa_data_specificx
              valid_date     = sy-datum
              valid_time     = sy-uzeit
            IMPORTING
              return         = gwa_return
            TABLES
              extensionin    = git_xtensionin.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          UNASSIGN <fl_log>.
          APPEND INITIAL LINE TO tg_log ASSIGNING <fl_log>.
          MOVE: cg_suce_id          TO <fl_log>-id.
          MOVE: el_return-type      TO <fl_log>-type.
          MOVE-CORRESPONDING <fl_alv> TO <fl_log>.
          MOVE: vl_mat_doc          TO <fl_log>-mat_doc,
                vl_doc_year         TO <fl_log>-doc_year.

          REFRESH lt_celltab.
          <fl_alv>-proces = 'X'.
          PERFORM fill_celltab USING 'RO'
                               CHANGING lt_celltab.

          <fl_alv>-celltab[] = lt_celltab[].

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = <fl_log>-matnr
            IMPORTING
              output = <fl_log>-matnr.
**
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          UNASSIGN <fl_log>.
          APPEND INITIAL LINE TO tg_log ASSIGNING <fl_log>.
          MOVE: cg_erro_id          TO <fl_log>-id.
          MOVE: el_return-type      TO <fl_log>-type.
          MOVE-CORRESPONDING <fl_alv> TO <fl_log>.
          MOVE: el_return-message   TO <fl_log>-observ.
          <fl_alv>-obs = el_return-message.
**
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = <fl_log>-matnr
            IMPORTING
              output = <fl_log>-matnr.
**
        ENDIF.
*              ELSE.
*                CONCATENATE 'Equipamento: ' <fl_alv>-equnr ' diferente de categoria M' INTO <fl_alv>-obs.
*              ENDIF.
*            ELSE.
*              CONCATENATE 'Equipamento: ' <fl_alv>-equnr ' com centro diferente de Material' INTO <fl_alv>-obs.
*            ENDIF.
*
*          ELSE.
*            CONCATENATE 'Equipamento: ' <fl_alv>-equnr ' já possui material atribuído, favor verificar' INTO <fl_alv>-obs.
*          ENDIF.
*
*        ELSE.
*          CONCATENATE 'Equipamento: ' <fl_alv>-equnr ' não foi encontrado na SAP' INTO <fl_alv>-obs.
*        ENDIF.
      ENDLOOP.

      IF NOT tg_log IS INITIAL.
        PERFORM zf_grava_log.
        CLEAR vg_code.
      ENDIF.
  ENDCASE.

  CALL METHOD og_alv->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

ENDFORM.                    "zf_handle_user_command
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA tl_exclude TYPE ui_functions.                        "#EC *

  DATA el_variant TYPE disvariant.                          "#EC *

  SET PF-STATUS 'ZALV_STANDARD'.

  IF NOT o_cont IS INITIAL.
    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ELSE.
    CREATE OBJECT o_cont
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_top
        repid     = sy-repid
        dynnr     = '0100'
        extension = 1000
      EXCEPTIONS
        OTHERS    = 6.

    o_parent = o_cont.

    CREATE OBJECT og_alv
      EXPORTING
        i_parent = o_parent.

    PERFORM: zf_monta_fieldcat,
             zf_layout,
             zf_exclude_tb_functions TABLES tl_exclude.

    el_variant-report   = sy-repid.
    el_variant-username = sy-uname.

    CREATE OBJECT v_event.
    SET HANDLER v_event->handle_toolbar
                v_event->handle_menu_button
                v_event->handle_user_command FOR ALL INSTANCES.

    eg_layout-cwidth_opt = 'X'.
    eg_layout-stylefname = 'CELLTAB'.

    CALL METHOD og_alv->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_exclude
        is_variant           = el_variant
        is_layout            = eg_layout
        i_save               = 'A'
      CHANGING
        it_outtab            = tg_alv
        it_fieldcatalog      = tg_fieldcat.

    CALL METHOD og_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD og_alv->set_toolbar_interactive.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  zf_monta_fieldcat
*&---------------------------------------------------------------------*
FORM zf_monta_fieldcat.

  DATA el_fieldcat TYPE lvc_s_fcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'PROCES'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Não Lançar'.
  el_fieldcat-checkbox  = abap_true.
  el_fieldcat-edit      = abap_true.
  el_fieldcat-outputlen = '10'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'ARQUIVO'.
  el_fieldcat-coltext   = 'Arquivo'.
  el_fieldcat-outputlen = '20'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'WERKS'.
  el_fieldcat-coltext   = 'Centro'.
  el_fieldcat-outputlen = '7'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'LGORT'.
  el_fieldcat-coltext   = 'Depósito'.
  el_fieldcat-outputlen = '8'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'MATNR'.
  el_fieldcat-coltext   = 'Material'.
  el_fieldcat-outputlen = '15'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'DESCR'.
  el_fieldcat-coltext   = 'Descrição'.
  el_fieldcat-outputlen = '30'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

* ZPP-CS2022000423 MelhoriasProcFerramentCargaIn-BG #76549     _Inicio
  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'EQUNR'.
  el_fieldcat-coltext   = 'Equipamento'.
  el_fieldcat-outputlen = '15'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'DESCR_EQ'.
  el_fieldcat-coltext   = 'Descrição'.
  el_fieldcat-outputlen = '30'.
  APPEND el_fieldcat TO tg_fieldcat.

* ZPP-CS2022000423 MelhoriasProcFerramentCargaIn-BG #76549  - FIM

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'LABST'.
  el_fieldcat-ref_table = 'MARD'.
  el_fieldcat-ref_field = 'LABST'.
  el_fieldcat-coltext   = 'Qtde'.
  el_fieldcat-outputlen = '8'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'MEINS'.
  el_fieldcat-ref_table = 'MARA'.
  el_fieldcat-ref_field = 'MEINS'.
  el_fieldcat-coltext   = 'Unid.'.
  el_fieldcat-outputlen = '4'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'TP_MOV'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Tp.Est.'.
  el_fieldcat-outputlen = '6'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'BLDAT'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Dt.Doc.'.
  el_fieldcat-outputlen = '10'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'LIFNR'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Fornecedor'.
  el_fieldcat-outputlen = '15'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'TXT_CAB'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Txt Cabeçalho'.
  el_fieldcat-outputlen = '40'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'REF_DOC'.
  el_fieldcat-coltext   = 'NF Material'.
  el_fieldcat-outputlen = '15'.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'OBS'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Observações'.
  el_fieldcat-outputlen = '60'.
  el_fieldcat-colddictxt = 'X'.
  el_fieldcat-selddictxt = 'X'.
  el_fieldcat-tipddictxt = 'X'.
  APPEND el_fieldcat TO tg_fieldcat.

ENDFORM.                    "zf_monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  zf_layout
*&---------------------------------------------------------------------*
FORM zf_layout.
  eg_layout-zebra = abap_true.
ENDFORM.                    " zf_layout
*&---------------------------------------------------------------------*
*&      Form  zf_exclude_tb_functions
*&---------------------------------------------------------------------*
FORM zf_exclude_tb_functions TABLES pt_exclude TYPE ui_functions.

  REFRESH pt_exclude.
  APPEND og_alv->mc_fc_detail            TO pt_exclude. "Botão Detalhe
  APPEND og_alv->mc_fc_refresh           TO pt_exclude. "Botão Refresh
  APPEND og_alv->mc_fc_loc_cut           TO pt_exclude. "Botão Recortar
  APPEND og_alv->mc_fc_loc_paste         TO pt_exclude. "Botão Colar com Sobregravação
  APPEND og_alv->mc_fc_loc_paste_new_row TO pt_exclude. "Botão Colar em Nova Linha
  APPEND og_alv->mc_fc_loc_copy_row      TO pt_exclude. "Botão Duplicar Linha
  APPEND og_alv->mc_fc_loc_append_row    TO pt_exclude. "Botão Anexar Linha
  APPEND og_alv->mc_fc_loc_insert_row    TO pt_exclude. "Botão Inserir Linha
  APPEND og_alv->mc_fc_loc_delete_row    TO pt_exclude. "Botão Deletar Linha
  APPEND og_alv->mc_fc_loc_copy          TO pt_exclude. "Botão Copiar Texto
  APPEND og_alv->mc_fc_loc_undo          TO pt_exclude. "Botão Anular
  APPEND og_alv->mc_fc_graph             TO pt_exclude. "Botão Grafico
  APPEND og_alv->mc_fc_info              TO pt_exclude. "Botão Help
  APPEND og_alv->mc_fc_send              TO pt_exclude.
  APPEND og_alv->mc_fc_separator         TO pt_exclude.
  APPEND og_alv->mc_fc_sort              TO pt_exclude.
  APPEND og_alv->mc_fc_sort_asc          TO pt_exclude.
  APPEND og_alv->mc_fc_sort_dsc          TO pt_exclude.
  APPEND og_alv->mc_fc_subtot            TO pt_exclude.
  APPEND og_alv->mc_fc_sum               TO pt_exclude.
  APPEND og_alv->mc_fc_to_office         TO pt_exclude.
  APPEND og_alv->mc_fc_to_rep_tree       TO pt_exclude.
  APPEND og_alv->mc_fc_unfix_columns     TO pt_exclude.
  APPEND og_alv->mc_fc_url_copy_to_clipboard   TO pt_exclude.
  APPEND og_alv->mc_fc_variant_admin     TO pt_exclude.
  APPEND og_alv->mc_fc_views             TO pt_exclude.
  APPEND og_alv->mc_fc_view_crystal      TO pt_exclude.
  APPEND og_alv->mc_fc_view_excel        TO pt_exclude.
  APPEND og_alv->mc_fc_view_grid         TO pt_exclude.
  APPEND og_alv->mc_fc_view_lotus        TO pt_exclude.
  APPEND og_alv->mc_ly_no_insert_rows    TO pt_exclude.
  APPEND og_alv->mc_mb_export            TO pt_exclude.
  APPEND og_alv->mc_mb_filter            TO pt_exclude.
  APPEND og_alv->mc_mb_paste             TO pt_exclude.
  APPEND og_alv->mc_mb_subtot            TO pt_exclude.
  APPEND og_alv->mc_mb_sum               TO pt_exclude.
  APPEND og_alv->mc_mb_variant           TO pt_exclude.
  APPEND og_alv->mc_mb_view              TO pt_exclude.
  APPEND og_alv->mc_fc_print             TO pt_exclude.
  APPEND og_alv->mc_fc_find              TO pt_exclude.
  APPEND og_alv->mc_fc_find_more         TO pt_exclude.

ENDFORM.                                        " EXCLUDE_TB_FUNCTIONS .
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR vg_code.
  vg_code = sy-ucomm.
  CASE vg_code.
    WHEN 'BACK' OR 'LEAVE'.
      CLEAR vg_code.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      CLEAR vg_code.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE zm_status_0200 OUTPUT.

  DATA: el_functions TYPE REF TO cl_salv_functions_list,    "#EC *
        cl_alv       TYPE REF TO cl_salv_table,             "#EC *
        cl_columns   TYPE REF TO cl_salv_columns_table,     "#EC *
        cl_column    TYPE REF TO cl_salv_column_table.      "#EC *

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = cl_alv
    CHANGING
      t_table      = tg_log ).

  el_functions = cl_alv->get_functions( ).
  el_functions->set_all( abap_true ).

  cl_columns = cl_alv->get_columns( ).

  cl_column ?= cl_columns->get_column( 'ID' ).
  cl_column->set_long_text( 'Status'(t17) ).
  cl_column->set_alignment( 4 ).
  cl_column->set_output_length( '5' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'MATNR' ).
  cl_column->set_long_text( 'Material'(t18) ).
  cl_column->set_output_length( '15' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'WERKS' ).
  cl_column->set_long_text( 'Centro'(t19) ).
  cl_column->set_output_length( '7' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'LGORT' ).
  cl_column->set_long_text( 'Depósito'(t20) ).
  cl_column->set_output_length( '7' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'LABST' ).
  cl_column->set_short_text( 'Qtde'(t21) ).
  cl_column->set_medium_text( 'Qtde'(t21) ).
  cl_column->set_long_text( 'Quantidade'(t28) ).
  cl_column->set_output_length( '8' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'MEINS' ).
  cl_column->set_short_text( 'Unid.'(t22) ).
  cl_column->set_long_text( 'Un.Medida'(t23) ).
  cl_column->set_output_length( '4' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'TP_MOV' ).
  cl_column->set_long_text( 'Tp.Mov.'(t24) ).
  cl_column->set_output_length( '10' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'MAT_DOC' ).
  cl_column->set_long_text( 'Mat.Doc.'(t25) ).
  cl_column->set_output_length( '13' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'DOC_YEAR' ).
  cl_column->set_long_text( 'Ano Doc.'(t26) ).
  cl_column->set_output_length( '7' ).
  cl_column->set_visible( abap_true ).

  cl_column ?= cl_columns->get_column( 'OBSERV' ).
  cl_column->set_long_text( 'Observação'(t27) ).
  cl_column->set_output_length( '80' ).
  cl_column->set_visible( abap_true ).

  IF cl_alv IS BOUND.
    cl_alv->display( ).
  ENDIF.

  CASE sy-ucomm.
    WHEN '&F15'.
      LEAVE PROGRAM.
    WHEN '&F12'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0100.
  ENDCASE.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE zm_user_command_0200 INPUT.

  DATA vl_code TYPE sy-ucomm.                               "#EC *
  vl_code = sy-ucomm.
  CASE vl_code.
    WHEN 'LEAVE'.
      CLEAR vl_code.
      LEAVE LIST-PROCESSING.
    WHEN 'CANCEL'.
      CLEAR vl_code.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR vl_code.
      SET SCREEN 0100.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " ZM_USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_LOG
*&---------------------------------------------------------------------*
FORM zf_grava_log.

  DATA: lt_zpmt0039 TYPE TABLE OF zpmt0039,
        ls_zpmt0039 TYPE zpmt0039.

  LOOP AT tg_log INTO eg_log.
    MOVE-CORRESPONDING eg_log TO ls_zpmt0039.
    APPEND ls_zpmt0039 TO lt_zpmt0039.
  ENDLOOP.
  IF lt_zpmt0039[] IS NOT INITIAL.
    MODIFY zpmt0039 FROM TABLE lt_zpmt0039.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_ALTERACAO_ARQUIVO
*&---------------------------------------------------------------------*
FORM zf_grava_alteracao_arquivo.

  TYPES: BEGIN OF yl_fieldnames,
           line TYPE c LENGTH 50,
         END   OF yl_fieldnames.

  DATA tl_fieldnames   TYPE STANDARD TABLE OF yl_fieldnames.

  FIELD-SYMBOLS:
    <fl_fieldnames> TYPE yl_fieldnames,
    <fl_alv>        TYPE yg_alv,
    <fl_alv_arq>    TYPE yg_alv.

  DEFINE title_colunas.
    UNASSIGN <fl_fieldnames>.
    APPEND INITIAL LINE TO tl_fieldnames ASSIGNING <fl_fieldnames>.
    MOVE &1 TO <fl_fieldnames>-line.
  END-OF-DEFINITION.

  REFRESH tl_fieldnames.
  title_colunas 'Material'(t01).
  title_colunas 'Centro'(t02).
  title_colunas 'Depósito'(t03).
  title_colunas 'Quantidade'(t04).
  title_colunas 'Un.Medida'(t05).
  title_colunas 'Tp.Movimento'(t07).
  title_colunas 'NF Material'(t08).
  title_colunas 'Processado'(t09).
  title_colunas 'Txt.Cabec.'(t29).

  UNASSIGN <fl_alv>.
  LOOP AT tg_alv ASSIGNING <fl_alv>.
    UNASSIGN <fl_alv_arq>.
    READ TABLE tg_alv_arq ASSIGNING <fl_alv_arq>
                          WITH KEY matnr    = <fl_alv>-matnr
                                   werks    = <fl_alv>-werks
                                   lgort    = <fl_alv>-lgort
                                   tp_mov   = <fl_alv>-tp_mov.
    IF <fl_alv_arq> IS ASSIGNED.
      MOVE: <fl_alv>-proces TO <fl_alv_arq>-proces.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename              = vg_file
      write_field_separator = abap_true
      write_lf              = abap_true
      show_transfer_status  = abap_true
    TABLES
      data_tab              = tg_alv_arq
      fieldnames            = tl_fieldnames.

ENDFORM.                                  " ZF_GRAVA_ALTERACAO_ARQUIVO .

FORM fill_celltab USING VALUE(p_mode)
                  CHANGING pt_celltab TYPE lvc_t_styl.
  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  IF p_mode EQ 'RW'.
*§2a.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_ENABLED to set a cell
*    to status "editable".
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE. "p_mode eq 'RO'
*§2b.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_DISABLED to set a cell
*    to status "non-editable".
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  ls_celltab-fieldname = 'PROCES'.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.


ENDFORM.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  F_IMPORTAR_ANEXOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_importar_anexos .

  FREE: t_anexos.
  CLEAR: l_obj_key, l_ip_service, w_bor, l_ip_service.

  l_obj_key = 'ZPM0075'.

  CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
    EXPORTING
      classname          = 'ZPM0075_ANEXO'
      objkey             = l_obj_key
      client             = sy-mandt
    TABLES
      gos_connections    = t_anexos
    EXCEPTIONS
      no_objects_found   = 1
      internal_error     = 2
      internal_gos_error = 3
      OTHERS             = 4.

  DESCRIBE TABLE t_anexos LINES l_lines.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  l_ip_mode     = 'E'.
  l_ip_service  = COND #( WHEN l_lines = 0 THEN 'PCATTA_CREA'
                                           ELSE 'VIEW_ATTA' ).
  w_bor-objkey  = l_obj_key. "l_chave.
  w_bor-objtype = 'ZPM0075_ANEX'.

  anexo_obj->set_rw_mode( ip_mode = l_ip_mode ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service         = l_ip_service
      is_object          = w_bor
    EXCEPTIONS
      no_object          = 1
      object_invalid     = 2
      execution_failed   = 3
      OTHERS             = 4 ).

  WAIT UP TO 2 SECONDS.

  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_estorno
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_estorno .

  DATA: lt_zpmt0039 TYPE TABLE OF zpmt0039.
  FIELD-SYMBOLS:
        <fl_alv>         TYPE yg_alv.


  DATA: ls_goodsmvt_head TYPE bapi2017_gm_head_ret,
        lt_return        TYPE TABLE OF bapiret2,
        el_return        TYPE bapiret2.

  DATA: vl_message  TYPE bapiret2-message.

*-----------------------------------------------------------

  IF pc_trans IS NOT INITIAL.
    IF tg_alv[] IS NOT INITIAL.

      LOOP AT tg_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_alv>-equnr
          IMPORTING
            output = <fs_alv>-equnr.

      ENDLOOP.

      SELECT * INTO TABLE lt_zpmt0039
      FROM zpmt0039
      FOR ALL ENTRIES IN tg_alv
      WHERE equnr = tg_alv-equnr AND type NE 'E'.

      SORT lt_zpmt0039 BY arquivo
                          werks
                          matnr
                          meins
                          lgort
                          bldat
                          lifnr
                          ref_doc.

      UNASSIGN <fl_alv>.
      LOOP AT tg_alv ASSIGNING <fl_alv>.

        READ TABLE lt_zpmt0039 INTO DATA(ls_zpmt0039)  WITH KEY equnr = <fl_alv>-equnr
                                                                    BINARY SEARCH.
        IF sy-subrc = 0 .

          SET UPDATE TASK LOCAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
            EXPORTING
              materialdocument    = ls_zpmt0039-mat_doc     "Número do doc. material
              matdocumentyear     = ls_zpmt0039-doc_year    "Ano
              goodsmvt_pstng_date = sy-datum                "Data de lançamento
              goodsmvt_pr_uname   = sy-uname                "Usuário
            IMPORTING
              goodsmvt_headret    = ls_goodsmvt_head
            TABLES
              return              = lt_return.


          READ TABLE lt_return INTO el_return
                               WITH KEY type = 'E'.
          IF NOT sy-subrc IS INITIAL.

            DELETE zpmt0039 FROM ls_zpmt0039.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

            MESSAGE s000(z_mm) DISPLAY LIKE 'S' WITH TEXT-034.

          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            MOVE: el_return-message   TO vl_message.
            MESSAGE vl_message TYPE 'S' DISPLAY LIKE 'E'.

            DATA(lv_erro) = abap_true.
            EXIT.

          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF lv_erro IS NOT INITIAL.
      REFRESH tg_alv.
    ENDIF.
  ENDIF.


ENDFORM.
