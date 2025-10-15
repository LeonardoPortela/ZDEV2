*&---------------------------------------------------------------------*
*& Report  ZGLT069
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zglt069.

TABLES: skb1,  ska1.   "US# 188476 - ROBPDIAS     ".

TYPES: BEGIN OF ty_saida,
         saknr             TYPE skb1-saknr, "CONTA RAZÃO
         bukrs             TYPE skb1-bukrs, "EMPRESA US# 188476 - ROBPDIAS
         txt50             TYPE skat-txt50, "Txt.descritivo
         txt30             TYPE t077z-txt30, "Txt.descritivo PLANO DE CONTAS
         ktoks             TYPE ska1-ktoks, "Código Grupo de contas US# 188476 - ROBPDIAS
         mitkz             TYPE skb1-mitkz, "Tipo de cntas US# 188476 - ROBPDIAS
         xspeb             TYPE skb1-xspeb, "STATUS da conta US# 188476 - ROBPDIAS
         name              TYPE thead-tdname,
         fsttx             TYPE t004g-fsttx, " Textp Grupo de contas US# 188476 - ROBPDIAS
         dep_resp2         LIKE zglt041-dep_resp2,"Código Departamento US# 188476 - ROBPDIAS
         dep_resp_desc     LIKE zimp_cad_depto-dep_resp_desc,"Descrição Departamento responsável US# 188476 - ROBPDIAS
         bname2            LIKE zglt041-bname2,"ID sap usuário Responsável US# 188476 - ROBPDIAS
         useralias         LIKE usrefus-useralias,"Nome usuário Responsável US# 188476 - ROBPDIAS
         cta_intercompany(8),
         cta_monet(7),
         chave11(40),
         chave12(40),
         criterio          TYPE char10,
         cod_clas_bal(40),
         cod_clas_not2(40),
       END OF ty_saida.

DATA: BEGIN OF it_zglt039 OCCURS 0,
        codigo     TYPE zglt039-codigo,
        cod_nota   TYPE zglt039-cod_nota,
        descr      TYPE zglt039-descr,
        descr_nota TYPE zglt039-descr_nota,
        "INCLUDE STRUCTURE ZGLT039.
        chave1(40),
        chave2(40),
        chave11(40),
        chave12(40).
DATA: END OF it_zglt039.


DATA: it_saida TYPE TABLE OF ty_saida,
      wa_saida TYPE ty_saida.

DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container            TYPE REF TO cl_gui_container,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      tg_selectedrow       TYPE lvc_t_row,
      wg_selectedrow       TYPE lvc_s_row,
      it_fcat              TYPE TABLE OF lvc_s_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      wa_stable            TYPE lvc_s_stbl VALUE 'XX'.

DATA: tg_texto TYPE TABLE OF tline WITH HEADER LINE,
      tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab.



INITIALIZATION.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: "P_SAKNR FOR SKA1-SAKNR.
                    p_saknr FOR skb1-saknr, "US# 188476 - ROBPDIAS
                    p_bukrs FOR skb1-bukrs. "US# 188476 - ROBPDIAS
  SELECTION-SCREEN END OF BLOCK b1.


CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_button_click01 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_button_click01.
    READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.
    CASE es_col_id.
      WHEN 'CRITERIO'.
        READ TABLE it_saida INTO wa_saida  INDEX es_row_no-row_id.

        REFRESH: tg_texto, tl_texto.
        CLEAR wl_texto.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'ZCRI'
            language                = sy-langu
            name                    = wa_saida-name
            object                  = 'ZCRITERIO'
          TABLES
            lines                   = tg_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF sy-subrc IS INITIAL.
          LOOP AT tg_texto INTO DATA(wa_texto).
            MOVE: wa_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Desrição Conta'
            im_display_mode = 'X'
          CHANGING
            ch_text         = tl_texto.
    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.


*  SELECT * FROM ska1 INTO TABLE @it_ska1 "#EC CI_DB_OPERATION_OK[2431747]
*   WHERE ktopl EQ '0050'               "#EC CI_DB_OPERATION_OK[2389136]
*    AND  saknr = @p_saknr   "US# 188476 - ROBPDIAS
*    AND  xspeb NE 'X'.

  SELECT skb1~*, ska1~ktoks FROM skb1
   INNER JOIN ska1 ON ska1~saknr = skb1~saknr
    AND ska1~xspeb <> 'X'
    AND ska1~ktopl = '0050'
   INTO TABLE @DATA(it_skb1)
  WHERE skb1~saknr IN @p_saknr
    AND skb1~bukrs IN @p_bukrs .


  SELECT * FROM  skat INTO TABLE @DATA(it_skat)
  WHERE spras EQ @sy-langu
    AND saknr IN @p_saknr
    AND ktopl EQ '0050'.

  SELECT * FROM t077s
    INTO TABLE  @DATA(it_t077s)
    FOR ALL ENTRIES IN @it_skb1
    WHERE ktopl EQ '0050'
    AND   ktoks EQ @it_skb1-ktoks.


  SELECT * FROM zglt041 INTO TABLE @DATA(it_zglt041)
  FOR ALL ENTRIES IN @it_skb1
  WHERE bukrs EQ @it_skb1-skb1-bukrs
  AND saknr EQ @it_skb1-skb1-saknr.

*US 188476 - ROBPDIAS
*Seleção Departamento Responsável.
  SELECT *
        FROM zimp_cad_depto
        INTO TABLE @DATA(tg_zimp_cad_depto)
         FOR ALL ENTRIES IN @it_zglt041
         WHERE dep_resp EQ @it_zglt041-dep_resp2.



  SELECT *
    FROM t004g INTO TABLE @DATA(it_t004g)
    FOR ALL ENTRIES IN @it_t077s
   WHERE spras EQ @sy-langu
    AND  fstag EQ @it_t077s-ktoks
    AND  bukrs EQ '0001'.
*Seleção Usuário Responsável
     SELECT *
      FROM USREFUS
      INTO TABLE @DATA(TG_USREFUS)
      FOR ALL ENTRIES IN @it_ZGLT041
       WHERE BNAME EQ @it_ZGLT041-bname2.

*US 188476 - ROBPDIAS

  SELECT   codigo
           cod_nota
           descr
           descr_nota
  FROM zglt039
  INTO TABLE it_zglt039.
*US 188476 - ROBPDIAS
  LOOP AT it_zglt039.
    IF it_zglt039-codigo IS NOT INITIAL.
      CONCATENATE it_zglt039-codigo '-' it_zglt039-descr INTO it_zglt039-chave11.
    ENDIF.
    IF it_zglt039-cod_nota IS NOT INITIAL.
      CONCATENATE it_zglt039-cod_nota '-' it_zglt039-descr_nota INTO it_zglt039-chave12.
    ENDIF.
    MODIFY it_zglt039.
  ENDLOOP.


  LOOP AT it_skb1 INTO DATA(wa_skb1).

    READ TABLE it_skat INTO DATA(wa_skat) WITH KEY saknr = wa_skb1-skb1-saknr.

    READ TABLE it_t077s INTO DATA(wa_t077s) WITH KEY ktoks = wa_skb1-ktoks.

    wa_saida-saknr = wa_skb1-skb1-saknr.
    wa_saida-txt30 = wa_t077s-ktoks.
    wa_saida-txt50 = wa_skat-txt50.

    READ TABLE it_t004g INTO DATA(wa_t004g) WITH KEY fstag = wa_t077s-ktoks.


    wa_saida-fsttx = wa_t004g-fsttx.


    wa_saida-mitkz = wa_skb1-skb1-mitkz.
    wa_saida-xspeb = wa_skb1-skb1-xspeb.

    READ TABLE it_zglt041 INTO DATA(wa_zglt041) WITH KEY saknr = wa_skb1-skb1-saknr
                                                         bukrs = wa_skb1-skb1-bukrs.
    IF sy-subrc = 0.


    wa_saida-bukrs = wa_zglt041-bukrs.

    READ TABLE it_zglt039  INTO DATA(wa_zglt039) WITH KEY codigo = wa_zglt041-cod_clas_bal.

    wa_saida-cod_clas_bal = wa_zglt039-chave1.

    READ TABLE it_zglt039 INTO wa_zglt039  WITH KEY cod_nota = wa_zglt041-cod_clas_not2.

    wa_saida-cod_clas_not2 = wa_zglt039-chave2.
    wa_saida-chave11 = wa_zglt039-chave11.
    wa_saida-chave12 = wa_zglt039-chave12.

    READ TABLE tg_zimp_cad_depto INTO DATA(wa_depto) WITH KEY   dep_resp = wa_zglt041-dep_resp2.

    wa_saida-dep_resp2 = wa_depto-dep_resp.
    wa_saida-dep_resp_desc = wa_depto-dep_resp_desc.

    READ TABLE TG_USREFUS INTO DATA(wa_usrefus) WITH KEY   BNAME = wa_zglt041-bname2.

    wa_saida-bname2 = wa_usrefus-bname.
    wa_saida-useralias = wa_usrefus-useralias.

    IF wa_zglt041-cta_intercompany EQ 'S'.
      wa_saida-cta_intercompany = 'S - SIM'.
    ELSEIF wa_zglt041-cta_intercompany EQ 'N'.
      wa_saida-cta_intercompany = 'N - NÃO'.
    ENDIF.

    IF wa_zglt041-cta_monet EQ 'S'.
      wa_saida-cta_monet = 'S - SIM'.
    ELSEIF wa_zglt041-cta_monet EQ 'N'.
      wa_saida-cta_monet = 'N - NÃO'.
    ENDIF.
*US 188476 - ROBPDIAS
    CONCATENATE wa_zglt041-bukrs wa_skb1-skb1-saknr wa_saida-cod_clas_bal wa_saida-cod_clas_not2 INTO wa_saida-name.



    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZCRI'
        language                = sy-langu
        name                    = wa_saida-name
        object                  = 'ZCRITERIO'
      TABLES
        lines                   = tg_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF tg_texto[] IS INITIAL.
      wa_saida-criterio =  '@1F@'.
    ELSE.
      wa_saida-criterio =    '@1E@'.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_skat, wa_t077s, wa_skb1, wa_zglt041, wa_usrefus, wa_depto, wa_zglt039, wa_t004g.
    REFRESH tg_texto.

ELSE.

        wa_saida-bukrs = wa_skb1-skb1-bukrs.

        CONCATENATE wa_skb1-skb1-bukrs wa_skb1-skb1-saknr INTO wa_saida-name.



            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ZCRI'
                language                = sy-langu
                name                    = wa_saida-name
                object                  = 'ZCRITERIO'
              TABLES
                lines                   = tg_texto
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

            IF tg_texto[] IS INITIAL.
              wa_saida-criterio =  '@1F@'.
            ELSE.
              wa_saida-criterio =    '@1E@'.
            ENDIF.




      APPEND wa_saida TO it_saida.
      CLEAR: wa_saida, wa_skat, wa_t077s, wa_skb1, wa_zglt041, wa_usrefus, wa_depto, wa_zglt039, wa_t004g.
      REFRESH tg_texto.

ENDIF.


  ENDLOOP.


  PERFORM alv.

  CALL SCREEN 0100.


FORM alv.

  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING :
  01 ''        ''      'IT_SAIDA'      'BUKRS'            'Empresa'                 '10'    ' '    ''     ''     ''    '',
  02 'SKB1'    'SAKNR' 'IT_SAIDA'      'SAKNR'            'Conta'                   '10'    'X'    ''     ''     ''    '',
  03 ''        ''      'IT_SAIDA'      'TXT50'            'Descrição'               '50'    ' '    ''     ''     ''    '',
  04 ''        ''      'IT_SAIDA'      'TXT30'            'Grupo Contas'            '30'    ' '    ''     ''     ''    '',
  05 ''        ''      'IT_SAIDA'      'FSTTX'            'Descrição Grupo Contas'  '20'    ' '    ''     ''     ''    '',
  06 ''        ''      'IT_SAIDA'      'XSPEB'            'STATUS DA CONTA'         '10'    ' '    ''     ''     ''    '',
  07 ''        ''      'IT_SAIDA'      'MITKZ'            'TIPO DE CONTA'           '10'    ' '    ''     ''     ''    '',
  08 'ZGLT039' 'DESCR' 'IT_SAIDA'      'CHAVE11'          'Classif.Balanço'         '30'    ' '    ''     ''     ''    '',
  09 ''        ''      'IT_SAIDA'      'CHAVE12'          'Classif.Nota'            '10'    ' '    ''     ''     ''    '',
  10 ''        ''      'IT_SAIDA'      'CTA_MONET'        'Monetária'               '10'    ' '    ''     ''     ''    '',
  11 ''        ''      'IT_SAIDA'      'CTA_INTERCOMPANY' 'Intercompany'            '10'    ' '    ''     ''     ''    '',
  12 ''        ''      'IT_SAIDA'      'DEP_RESP2'        'Departamento'            '15'    ' '    ''     ''     ''    '',
  13 ''        ''      'IT_SAIDA'      'DEP_RESP_DESC'    'Desc.Departamento'       '20'    ' '    ''     ''     ''    '',
  14 ''        ''      'IT_SAIDA'      'BNAME2'           'Responsável'             '20'    ' '    ''     ''     ''    '',
  15 ''        ''      'IT_SAIDA'      'USERALIAS'        'Desc.Responsável'        '20'    ' '    ''     ''     ''    '',
  16 ''        ''      'IT_SAIDA'      'CRITERIO'         'Descrição Conta'         '12'    ' '    ''     ''     ''    ''.

ENDFORM.

FORM preenche_cat USING VALUE(p_col_pos) type i
                        VALUE(p_ref_tabname)   LIKE dd02d-tabname
                        VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                        VALUE(p_tabname)       LIKE dd02d-tabname
                        VALUE(p_field)         LIKE dd03d-fieldname
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor).
  wl_fcat-fieldname = p_field.
  wl_fcat-tabname   = p_tabname.
  wl_fcat-ref_table   = p_ref_tabname.
  wl_fcat-ref_field   = p_ref_fieldname.
  wl_fcat-col_pos     = p_col_pos.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.

  IF wl_fcat-fieldname = 'SAKNR'.
    wl_fcat-no_zero = Abap_True.
    wl_fcat-convexit = 'ALPHA'.
  ELSE.
    CLEAR: wl_fcat-convexit.
  ENDIF.

  IF p_field EQ 'CRITERIO'.
    wl_fcat-style = cl_gui_alv_grid=>mc_style_button.
  ELSE.
    wl_fcat-style = ''.
  ENDIF.

CASE wl_fcat-fieldname.
  WHEN 'chave11'.
    wl_fcat-ref_table = 'ZGLT039'.
    wl_fcat-ref_field = 'DESCR'.

  WHEN OTHERS.
ENDCASE.

  APPEND wl_fcat TO  it_fcat.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: wa_event    TYPE REF TO lcl_event_handler.


  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid IS INITIAL AND  g_custom_container IS NOT INITIAL.
      CREATE OBJECT g_grid
        EXPORTING
          i_parent          = g_custom_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    IF wa_event IS INITIAL.
      CREATE OBJECT wa_event.
      SET HANDLER: wa_event->on_button_click01  FOR g_grid.
    ENDIF.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
