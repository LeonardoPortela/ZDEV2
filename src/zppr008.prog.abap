**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( cleudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Instrução                                                     |*
**/===========================================================================\*

REPORT zppr008.


TABLES: mchb,
        zppt0011. "RJF - #153637

TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zppt0011.
TYPES:
         maktx TYPE makt-maktx,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE char50,
         vfdai TYPE vfdat,
         color TYPE lvc_t_scol,
       END OF ty_saida,

       ty_t_saida         TYPE TABLE OF ty_saida WITH DEFAULT KEY,
       ty_t_dismemberment TYPE TABLE OF zppt0011 WITH DEFAULT KEY.

DATA gt_outtab TYPE TABLE OF ty_saida.
DATA document TYPE REF TO cl_dd_document.

DATA data_venc TYPE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-i01.
  SELECT-OPTIONS:
  s_matnr FOR mchb-matnr NO-EXTENSION NO INTERVALS OBLIGATORY,
  s_werks FOR mchb-werks NO-EXTENSION NO INTERVALS OBLIGATORY,
  s_lgort FOR mchb-lgort NO-EXTENSION NO INTERVALS OBLIGATORY,
  s_lfabr FOR zppt0011-lfabr OBLIGATORY NO-EXTENSION NO INTERVALS. "RJF - #153637
*  s_charg FOR mchb-charg NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
*
CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS run.

    METHODS set_title_and_status.

    METHODS select_data
      EXCEPTIONS
        data_not_found.

    METHODS get_dismemberments
      RETURNING VALUE(table) TYPE ty_t_dismemberment.

    METHODS get_pedidos
      RETURNING VALUE(table) TYPE ekko_tty.

    METHODS set_header.
    METHODS create_docking.

    METHODS get_material_description
      IMPORTING
        material    TYPE makt-matnr
      RETURNING
        VALUE(text) TYPE makt-maktx.

    METHODS get_vfdat_begin
      IMPORTING
        material     TYPE matnr
        charg        TYPE charg_d
      RETURNING
        VALUE(vfdat) TYPE cdobjectv.

    METHODS get_werks_description
      IMPORTING
        werks       TYPE t001w-werks
      RETURNING
        VALUE(text) TYPE t001w-name1.

    METHODS get_lifnr_description
      IMPORTING
*        PEDIDO      TYPE EKKO_TTY
        ebeln       TYPE vbeln
      RETURNING
        VALUE(text) TYPE lfa1-name1.

    METHODS vfdat
      IMPORTING
        selected_rows TYPE lvc_t_row.

    METHODS get_fieldcatalog
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.

    METHODS process_before_output.
    METHODS set_outtab_data.

    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS display.

    CLASS-METHODS get_charg RETURNING VALUE(_charg) TYPE charg_d.

  PRIVATE SECTION.

*    "//tables
*  VFDAI
    DATA dismemberments         TYPE TABLE OF zppt0011.
    DATA pedidos                TYPE TABLE OF ekko.

*    "//Objects
    DATA docking        TYPE REF TO cl_gui_docking_container.
    DATA splitter       TYPE REF TO cl_gui_splitter_container.
    DATA custom_header  TYPE REF TO cl_gui_container.
    DATA custom_grid    TYPE REF TO cl_gui_container.
    DATA grid           TYPE REF TO cl_gui_alv_grid.
    DATA alv_tree       TYPE REF TO cl_gui_alv_tree.
*
ENDCLASS.

DATA r_main TYPE REF TO cl_main.

CLASS cl_main IMPLEMENTATION.

  METHOD run.

    CREATE OBJECT r_main.

    r_main->select_data( EXCEPTIONS data_not_found = 4 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ELSE.
      CALL SCREEN 0001.
    ENDIF.
  ENDMETHOD.

  METHOD process_before_output.
    "//set title
    me->set_title_and_status( ).

    "//screen components
    me->create_docking( ).

    "//set data
    me->set_header( ).
    me->set_outtab_data( ).

    "//display data
    me->display( ).
  ENDMETHOD.

  METHOD set_title_and_status.
    SET TITLEBAR 'MAIN_TITLE'.
    SET PF-STATUS 'MAIN_STATUS'.
  ENDMETHOD.

  METHOD select_data.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-low
      IMPORTING
        output       = s_matnr-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-high
      IMPORTING
        output       = s_matnr-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE *
      FROM zppt0011
      INTO @DATA(_pedido)
     WHERE lfabr IN @s_lfabr.

    IF sy-subrc IS INITIAL.

      SELECT *
        FROM zppt0011 AS a
        INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
       WHERE a~matnr IN s_matnr
         AND a~werks IN s_werks
         AND a~lgort IN s_lgort
         AND a~lfabr EQ _pedido-lfabr.
*         and a~vfdat lt sy-datum "RJF - #153637
*         AND a~clabs GT 0.  "RJF - #153637
*      if sy-subrc ne 0.
*        free: me->dismemberments.
*
*        select * from zppt0016
*          into table @data(tl_zppt0016)
*          where matnr   in @s_matnr
*            and werks   in @s_werks
**          and vfdat   in @s_vfdat
*            and zlicha  in @s_lfabr
*            and lgort   in @s_lgort.
**          and charg   in @s_charg.
*
*        if sy-subrc is initial.
*          me->dismemberments =
*              value #( for ls in tl_zppt0016
*                           (
*                               matnr = ls-matnr
*                               werks = ls-werks
*                               lgort = ls-lgort
*                               charg = ls-chargd
*                               clabs = ''
*                               vfdat = ls-vfdat
*                               lfabr = ls-zlicha
*                               ebeln = '' ) ).
*
*
*        endif.
*      endif.
    ELSE.

      SELECT *
        FROM zppt0011 AS a
        INTO CORRESPONDING FIELDS OF TABLE me->dismemberments
       WHERE a~matnr IN s_matnr
         AND a~werks IN s_werks
         AND a~lgort IN s_lgort
*         AND a~lfabr IN s_charg  "RJF - #153637
*         and a~charg in s_charg  "RJF - #153637
         AND a~lfabr IN s_lfabr.   "RJF - #153637
*         and a~vfdat lt sy-datum "RJF - #153637
*         AND a~clabs GT 0.       "RJF - #153637

      IF sy-subrc NE 0.
        FREE: me->dismemberments.

        SELECT * FROM zppt0016
          INTO TABLE @DATA(tl_zppt0016)
          WHERE matnr   IN @s_matnr
            AND werks   IN @s_werks
*          and vfdat   in @s_vfdat
            AND zlicha  IN @s_lfabr
            AND lgort   IN @s_lgort.
*          and charg   in @s_charg.

        IF sy-subrc IS INITIAL.
          me->dismemberments =
              VALUE #( FOR ls IN tl_zppt0016
                           (
                               matnr = ls-matnr
                               werks = ls-werks
                               lgort = ls-lgort
                               charg = ls-chargd
                               clabs = ''
                               vfdat = ls-vfdat
                               lfabr = ls-zlicha
                               ebeln = '' ) ).


        ENDIF.
      ENDIF.

    ENDIF.

    IF me->dismemberments IS NOT INITIAL.
      SELECT *
        FROM ekko
        INTO TABLE me->pedidos
       FOR ALL ENTRIES IN me->dismemberments
         WHERE ebeln = me->dismemberments-ebeln.
    ELSE.
      MESSAGE TEXT-e01 TYPE 'S' RAISING data_not_found.
    ENDIF.


  ENDMETHOD.
*
  METHOD get_dismemberments.
    MOVE me->dismemberments TO table.
  ENDMETHOD.

  METHOD get_pedidos.
    MOVE me->pedidos TO table.
  ENDMETHOD.


  METHOD set_header.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-low
      IMPORTING
        output       = s_matnr-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-high
      IMPORTING
        output       = s_matnr-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    DATA table_element  TYPE REF TO cl_dd_table_element.
    DATA table_texts     TYPE sdydo_text_table.
    DATA table_text     LIKE LINE OF table_texts.
    DATA column         TYPE REF TO cl_dd_area.

    IF ( document IS NOT BOUND ).
      CREATE OBJECT document.
    ELSE.
      document->initialize_document( ).
    ENDIF.

    "//Build title text
    document->add_text( text = 'Parâmetros de seleção:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    document->new_line(  ). document->underline( ).

    document->add_table(
      EXPORTING
        no_of_columns = 2     " Number of Table Columns
        border        = '0'     " Width of Table Frame; '0' = No Frame
      IMPORTING
        table         = DATA(_doctable1)
    ).

    IF sy-subrc <> 0.

    ENDIF.

    _doctable1->add_column( EXPORTING width = '30%' IMPORTING column = DATA(_column_key) ).
    _doctable1->add_column( IMPORTING column = DATA(_column_value) ).

    _column_key->add_text( text = 'MATERIAL:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    _column_value->add_text( text = | { COND #( WHEN s_matnr-high IS INITIAL AND s_matnr-low IS NOT INITIAL
                                       THEN |{ CONV i( s_matnr-low ) } - { me->get_material_description( s_matnr-low ) }|
                                       WHEN s_matnr-high IS NOT INITIAL
                                       THEN |{ s_matnr-low } até { s_matnr-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'CENTRO:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    _column_value->add_text( text = | { COND #( WHEN s_werks-high IS INITIAL AND s_werks-low IS NOT INITIAL
                                       THEN |{ s_werks-low } - { me->get_werks_description( s_werks-low ) }|
                                       WHEN s_werks-high IS NOT INITIAL
                                       THEN |{ s_werks-low } até { s_matnr-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

    _column_key->add_text( text = 'DEPOSITO:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
    _column_value->add_text( text = | { COND #( WHEN s_lgort-high IS INITIAL AND s_lgort-low IS NOT INITIAL
                                       THEN |{ s_lgort-low }|
                                       WHEN s_lgort-high IS NOT INITIAL
                                       THEN |{ s_lgort-low } até { s_lgort-high }|
                                       ELSE '*' ) }| ).

    _doctable1->new_row( ).

*    _column_key->add_text( text = 'LOTE:' sap_fontsize = cl_dd_area=>large sap_emphasis = cl_dd_area=>emphasis ).
*    _column_value->add_text( text = | { cond #( when s_charg-high is initial and s_charg-low is not initial
*                                       then |{ s_charg-low }|
*                                       when s_charg-high is not initial
*                                       then |{ s_charg-low } até { s_charg-high }|
*                                       else '*' ) }| ).

    _doctable1->new_row( ).

    document->merge_document( ).
    document->display_document( parent = custom_header ).

  ENDMETHOD.
*
  METHOD create_docking.

    CREATE OBJECT docking
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = cl_gui_docking_container=>dock_at_top
        extension = 5000.

    CREATE OBJECT splitter
      EXPORTING
        link_dynnr = sy-dynnr
        link_repid = sy-repid
        top        = 50
        parent     = docking
        rows       = 2
        columns    = 1.

    me->splitter->set_row_height( id = 1 height = 15 ).

    custom_header = me->splitter->get_container( row = 1 column = 1 ).
    custom_grid   = me->splitter->get_container( row = 2 column = 1 ).

  ENDMETHOD.
*
  METHOD set_outtab_data.
    DATA outtab TYPE ty_saida.
*
    DATA(_dismemberments) = me->get_dismemberments( ).

    gt_outtab = VALUE #( FOR _dismemberment IN _dismemberments
                                (
                                    clabs = _dismemberment-clabs
                                    ebeln = _dismemberment-ebeln
                                    matnr = _dismemberment-matnr
                                    werks = _dismemberment-werks
                                    lgort = _dismemberment-lgort
                                    charg = _dismemberment-charg
                                    lfabr = _dismemberment-lfabr
                                    integrado = _dismemberment-integrado
                                    vfdai = me->get_vfdat_begin( material = _dismemberment-matnr charg = _dismemberment-charg )
                                    vfdat = _dismemberment-vfdat
                                    umcha = _dismemberment-umcha
                                    rsnum = _dismemberment-rsnum
                                    maktx = me->get_material_description( _dismemberment-matnr )
                                    name1 = me->get_lifnr_description( _dismemberment-ebeln )
                                )
                       ).
  ENDMETHOD.

  METHOD handle_set_toolbar.

    DATA(_standard_toolbars) = e_object->mt_toolbar.
    CLEAR e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'VLDAT'
                    icon      = icon_planning_out
                    text      = 'Alterar Vencimento'
                  ) TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.
    CALL METHOD me->grid->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'VLDAT'.
        me->vfdat( selected_row ).
    ENDCASE.
  ENDMETHOD.

  METHOD display.
    DATA(_fieldcatalog) = me->get_fieldcatalog( ).

    DATA(_layout) = VALUE lvc_s_layo( ctab_fname = 'COLOR' sel_mode = 'A' ).

    CREATE OBJECT me->grid
      EXPORTING
        i_parent = me->custom_grid.

    SET HANDLER: me->handle_set_toolbar  FOR me->grid,
                 me->handle_user_command FOR me->grid.

    CALL METHOD me->grid->set_table_for_first_display
      EXPORTING
        is_layout       = _layout
      CHANGING
        it_outtab       = gt_outtab
        it_fieldcatalog = _fieldcatalog.

    CALL METHOD me->grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ENDMETHOD.

  METHOD get_material_description.

    DATA: lv_material TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = lv_material
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT SINGLE maktx FROM makt INTO text WHERE matnr = lv_material.
  ENDMETHOD.

  METHOD get_vfdat_begin.

    DATA: _objid TYPE cdobjectv,
          _matnr TYPE matnr,
          _charg TYPE charg_d.

*    _matnr = |{ material ALPHA = IN }|.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = material
      IMPORTING
        output       = _matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    _charg = charg.

    _objid = |{ _matnr }    { _charg }|.

    SELECT *
      FROM cdhdr
       INTO TABLE @DATA(_vldt)
       WHERE objectid EQ @_objid
     ORDER BY udate, utime.

    TRY.
        vfdat = _vldt[ 1 ]-udate.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDMETHOD.

  METHOD get_werks_description.
    SELECT SINGLE name1 FROM t001w INTO text WHERE werks = werks.
  ENDMETHOD.

  METHOD get_lifnr_description.

    DATA(_pedidos) = me->get_pedidos( ).

    TRY .
        DATA(lifnr) = _pedidos[ ebeln = ebeln ]-lifnr.
      CATCH cx_sy_itab_line_not_found.
        CLEAR lifnr.
        EXIT.
    ENDTRY.

    SELECT SINGLE name1 FROM lfa1 INTO text WHERE lifnr = lifnr.
    text = |{ lifnr }-{ text }|.

  ENDMETHOD.

  METHOD vfdat.

    DATA: _bapibatchatt  TYPE bapibatchatt.
    DATA: _bapibatchattx TYPE bapibatchattx.
    DATA: _bapibatchkey  TYPE bapibatchkey.
    DATA  _bapiret2      TYPE TABLE OF bapiret2.
*-US 153637-28-10-2024-#153637-RJF-Inicio
    DATA: it_venc_pack TYPE zttsys_strut,
          wa_venc_pack TYPE zesys_strut.
*-US 153637-28-10-2024-#153637-RJF-Fim

    IF ( selected_rows IS INITIAL ).
      MESSAGE TEXT-i02 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.

      CALL SCREEN 0105 STARTING AT 30 05 ENDING AT 63 1.
      IF sy-ucomm EQ 'CANCELAR'.
        EXIT.
      ENDIF.

      DATA(_dismemberments) = me->get_dismemberments( ).

      LOOP AT selected_rows INTO DATA(_row).

        ASSIGN _dismemberments[ _row-index ] TO FIELD-SYMBOL(<dismemberments>).

*-US 153637-28-10-2024-#153637-RJF-Inicio
        DATA(lv_lfabr) = <dismemberments>-lfabr.
        LOOP AT _dismemberments ASSIGNING <dismemberments> WHERE lfabr EQ lv_lfabr.
*-US 153637-28-10-2024-#153637-RJF-Fim

          _bapibatchkey = VALUE #(
*                                  MATERIAL = <DISMEMBERMENTS>-MATNR
                                    batch = <dismemberments>-charg
                                    plant = <dismemberments>-werks
                                 ).

* ---> S4 Migration - 07/07/2023 - RZ - Inicio
*        DATA(v_len) = strlen( <dismemberments>-matnr ).

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = <dismemberments>-matnr
            IMPORTING
              output       = _bapibatchkey-material
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
*        IF v_len > 18.
*          _bapibatchkey-material_long  =   <dismemberments>-matnr.
*        ELSE.
*          _bapibatchkey-material       =   <dismemberments>-matnr.
*        ENDIF.
* <--- S4 Migration - 07/07/2023 - RZ - Fim

          _bapibatchatt = VALUE #( expirydate = data_venc ).
          _bapibatchattx = VALUE #( expirydate = abap_true ).

          "Campos de material tratados. Pseudo comentário adicionado     " >> ---> S4 Migration - 07/07/2023 - RZ
          CALL FUNCTION 'BAPI_BATCH_CHANGE'     "#EC CI_USAGE_OK[2438131] " >> ---> S4 Migration - 07/07/2023 - RZ
            EXPORTING
              material         = _bapibatchkey-material
              batch            = _bapibatchkey-batch
              plant            = _bapibatchkey-plant
              material_long    = _bapibatchkey-material_long " >> ---> S4 Migration - 07/07/2023 - RZ
              batchattributes  = _bapibatchatt
              batchattributesx = _bapibatchattx
            TABLES
              return           = _bapiret2.

          IF _bapiret2 IS INITIAL.

**-US 153637-28-10-2024-#153637-RJF-Inicio
*            UPDATE zppt0011
*              SET vfdat = data_venc
*                    WHERE matnr EQ _bapibatchkey-material
*                      AND werks EQ _bapibatchkey-plant
*                      AND charg EQ _bapibatchkey-batch.


*            update zppt0016
*              set vfdat = data_venc
*                    where matnr eq _bapibatchkey-material
*                      and werks eq _bapibatchkey-plant
*                      and charg eq _bapibatchkey-batch.
*
            UPDATE zppt0015
              SET vencimento = data_venc
                    WHERE material EQ _bapibatchkey-material
                      AND centro   EQ _bapibatchkey-plant
                      AND lote_individual    EQ _bapibatchkey-batch.
**-US 153637-28-10-2024-#153637-RJF-Fim

            "Atualiza tabela ZPPT0018 para envio de nova data para 'Defensivos'.
            DATA(_zppt0018) = VALUE zppt0018(
                matnr           = <dismemberments>-matnr
                werks           = <dismemberments>-werks
                lgort           = <dismemberments>-lgort
                charg           = <dismemberments>-charg
                vfdat           = data_venc
                reg_atualizado  = ''
                data_atual      = sy-datum
                hora_atual      = sy-uzeit
                usnam           = sy-uname
            ).

            MODIFY zppt0018 FROM _zppt0018.

            COMMIT WORK.

*    DATA: it_venc_pack TYPE zttsys_strut,
*          wa_venc_pack TYPE zesys_strut.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = <dismemberments>-matnr
              IMPORTING
                output = wa_venc_pack-material.

*            wa_venc_pack-material   = <dismemberments>-matnr.
            wa_venc_pack-centro     = <dismemberments>-werks.
            wa_venc_pack-deposito   = <dismemberments>-lgort.
            wa_venc_pack-lote       = <dismemberments>-lfabr.
            wa_venc_pack-vencimento = data_venc(4) && '-' && data_venc+4(2) && '-' && data_venc+6(2) && ' 00:00:00.000'.
            APPEND wa_venc_pack TO it_venc_pack.
            FREE wa_venc_pack.

          ENDIF.

**-US 153637-28-10-2024-#153637-RJF-Inicio
        ENDLOOP.

        UPDATE zppt0011
          SET vfdat = data_venc
                WHERE werks EQ _bapibatchkey-plant
                  AND matnr EQ _bapibatchkey-material
                  AND lfabr EQ <dismemberments>-lfabr.

        UPDATE zppt0016
          SET vfdat = data_venc
                WHERE  werks EQ _bapibatchkey-plant
                   AND zlicha EQ <dismemberments>-lfabr
                   AND matnr EQ _bapibatchkey-material
                   AND werks EQ _bapibatchkey-plant.
*                      and charg eq _bapibatchkey-batch.

        COMMIT WORK AND WAIT.
**-US 153637-28-10-2024-#153637-RJF-Fim

      ENDLOOP.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

**CS2024000871 -Alteração de data de vencimento Embalagens Amaggi Pack - VALIDAÇÃO LOTE FABRICANTE - PSA
      SELECT SINGLE * FROM zppt0011 WHERE lfabr = @<dismemberments>-lfabr INTO @DATA(ls_check_zppt0011).
      IF sy-subrc = 0 .

**-us 153637-28-10-2024-#153637-rjf-Inicio
        TRY.
            DATA: it_req_venc TYPE zde_req_venc. " estrutura envio

            IF it_venc_pack[] IS NOT INITIAL.
              SORT it_venc_pack[] BY material
                                     centro
                                     deposito
                                     lote
                                     vencimento.
              DELETE ADJACENT DUPLICATES FROM it_venc_pack COMPARING ALL FIELDS.

              it_req_venc-query = '#'.
              it_req_venc-variables-input-vencimentos[] = it_venc_pack[].

              BREAK rfreitas.

              zcl_int_ob_magi_pack_venc=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = it_req_venc
                                                                                                   IMPORTING e_integracao = DATA(lwa_integracao) ).
              CHECK lwa_integracao-ds_data_retorno IS NOT INITIAL.

              LOOP AT selected_rows INTO _row.
                ASSIGN _dismemberments[ _row-index ] TO <dismemberments>.

                UPDATE zppt0011
                  SET integrado = abap_true
                  WHERE werks EQ <dismemberments>-werks
                    AND matnr EQ <dismemberments>-matnr
                    AND lfabr EQ <dismemberments>-lfabr.
              ENDLOOP.
              COMMIT WORK AND WAIT.

            ENDIF.
          CATCH zcx_integracao INTO DATA(lwa_zcx_integracao).
            MESSAGE ID lwa_zcx_integracao->msgid TYPE 'I' NUMBER lwa_zcx_integracao->msgno WITH lwa_zcx_integracao->msgv1 lwa_zcx_integracao->msgv2 lwa_zcx_integracao->msgv3 lwa_zcx_integracao->msgv4.
          CATCH zcx_error INTO DATA(zcx_error).
            MESSAGE ID zcx_error->msgid TYPE 'I' NUMBER zcx_error->msgno WITH zcx_error->msgv1 zcx_error->msgv2 zcx_error->msgv3 zcx_error->msgv4.
        ENDTRY.
**-US 153637-28-10-2024-#153637-RJF-Fim

        ELSE.

          MESSAGE 'Registros alterados no SAP sem atualização Amaggi Pack' TYPE 'I'.

      ENDIF.

    ENDIF.

    me->select_data( EXCEPTIONS data_not_found = 4 ).
    me->set_outtab_data( ).

    CALL METHOD me->grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_fieldcatalog.

    fcat =
        VALUE #(
        ( fieldname = 'EBELN' coltext = 'Pedido'           outputlen = 12 )
        ( fieldname = 'MATNR' coltext = 'Material'         outputlen = 10 no_zero = abap_true )
        ( fieldname = 'MAKTX' coltext = 'Descrição'        outputlen = 25 )
        ( fieldname = 'WERKS' coltext = 'Centro'           outputlen = 07 )
        ( fieldname = 'LGORT' coltext = 'Depósito'         outputlen = 10 )
        ( fieldname = 'NAME1' coltext = 'Fornecedor'       outputlen = 25 no_zero = abap_true )
        ( fieldname = 'CHARG' coltext = 'Lote Amaggi'      outputlen = 13 )
        ( fieldname = 'LFABR' coltext = 'Lote Fabricante'  outputlen = 13 )
        ( fieldname = 'INTEGRADO' coltext = 'Integrado?'  outputlen = 13 )
*        ( fieldname = 'VFDAI' coltext = 'Validade Inicial' outputlen = 10 )
        ( fieldname = 'VFDAT' coltext = 'Validade Atual'   outputlen = 10 )
        ( fieldname = 'CLABS' coltext = 'Quantidade'       outputlen = 10 ) ).

  ENDMETHOD.

  METHOD get_charg.

    TYPES BEGIN OF ty_f4.
    TYPES charg TYPE charg_d.
    TYPES END OF ty_f4.

    DATA: it_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc.
    DATA: help_f4   TYPE TABLE OF ty_f4.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-low
      IMPORTING
        output       = s_matnr-low
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = s_matnr-high
      IMPORTING
        output       = s_matnr-high
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.

    SELECT charg
      FROM zppt0011
      INTO TABLE help_f4
     WHERE matnr IN s_matnr
       AND werks IN s_werks
       AND lgort IN s_lgort
       AND vfdat LT sy-datum
       AND clabs GT 0.

    SELECT lfabr
      FROM zppt0011
      APPENDING  TABLE help_f4
     WHERE matnr IN s_matnr
       AND werks IN s_werks
       AND lgort IN s_lgort
       AND vfdat LT sy-datum
       AND clabs GT 0.

    SORT help_f4.
    DELETE ADJACENT DUPLICATES FROM help_f4 COMPARING ALL FIELDS.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CHARG'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = help_f4
        return_tab      = it_return
        dynpfld_mapping = tl_dselc.

    CHECK it_return IS NOT INITIAL.

    _charg = it_return[ 1 ]-fieldval.

  ENDMETHOD.

ENDCLASS.

*at selection-screen on value-request for s_charg-low.
*  s_charg-low = cl_main=>get_charg( ).

START-OF-SELECTION.
  cl_main=>run( ).

*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pbo OUTPUT.
  IF r_main IS INITIAL.
    CREATE OBJECT r_main.
  ENDIF.

  r_main->process_before_output( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai INPUT.
  IF sy-ucomm = 'BACK'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0105 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.

      DATA: p_resp.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Deseja Alterar o Vencimento da Seleção?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        IMPORTING
          answer                = p_resp.

      IF p_resp EQ 1.
        IF data_venc IS INITIAL.
          MESSAGE TEXT-i03 TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
*          >>>>>>Inicio ajuste validar data venc bug #164353 / AOENNING
*          IF data_venc => sy-datum.
            LEAVE TO SCREEN 0.
*          ELSE.
*            MESSAGE TEXT-i04 TYPE 'S' DISPLAY LIKE 'E'.
*          ENDIF.
**         >>>>>>Fim ajuste validar data venc bug #164353 / AOENNING
        ENDIF.
      ELSE.
        MESSAGE TEXT-i05 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0105 OUTPUT.
  SET TITLEBAR '0105_TITLE'.
  SET PF-STATUS '0105_STATUS'.
ENDMODULE.
