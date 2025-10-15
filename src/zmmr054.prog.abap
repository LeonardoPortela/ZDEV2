*&---------------------------------------------------------------------*
*& Report  ZMMR054
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr054.
TABLES: resb, rkpf, pa0465.

TYPES:
  BEGIN OF ty_saida,


    matnr           TYPE zmmt0087-matnr,
    maktx           TYPE makt-maktx,
    numero_ca       TYPE zmme_ficha_controle_epi-numero_ca,

    rsnum           TYPE zmmt0087-rsnum,
    rspos           TYPE zmmt0087-rspos,
*  MATNR         TYPE ZMMT0087-MATNR,
    mblnr           TYPE zmmt0087-mblnr,
*  OPERACAO      TYPE ZMMT0087-OPERACAO,
    count_devolucao TYPE zmmt0087-count_devolucao,
    operacao        TYPE zmmt0087-operacao,
    quant           TYPE zmmt0087-quant,
    pernr           TYPE zmmt0087-pernr,
    data            TYPE zmmt0087-data,
    returned_qty    TYPE zmmt0087-quant,
    node            TYPE lvc_nkey,
    quant_to_return TYPE zmmt0087-quant,
    hash_validation TYPE string,
    lado_log        TYPE zmmt0081-lado,
    polegar_log     TYPE zmmt0081-polegar,
    im_polegar_log  TYPE zmmt0081-im_polegar,
  END OF ty_saida.


TYPES:
  ty_t_fichas TYPE TABLE OF zmme_ficha_controle_epi WITH DEFAULT KEY,
  ty_t_saida  TYPE TABLE OF ty_saida WITH EMPTY KEY,
  t_zmmt0087  TYPE TABLE OF zmmt0087 WITH DEFAULT KEY.

DATA quantity_to_return   TYPE zmmt0087-quant.
DATA senha_to_return      TYPE zmmt0120-senha.
DATA employee      TYPE zhcms_intranet_employee_data.
DATA custom_photo  TYPE REF TO cl_gui_custom_container.
DATA picture       TYPE REF TO cl_gui_picture.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-008.

  SELECT-OPTIONS:
    p_pernr FOR pa0465-pernr NO-EXTENSION NO INTERVALS OBLIGATORY MATCHCODE OBJECT /bev3/chpremn,
    p_rsnum FOR resb-rsnum,
    p_werks FOR resb-werks,
    p_matnr FOR resb-matnr,
    p_date  FOR rkpf-rsdat,
    p_kostl FOR rkpf-kostl.
SELECTION-SCREEN END OF BLOCK b1.

CLASS cl_main DEFINITION.
  PUBLIC SECTION.

    METHODS run.
    METHODS select_reservations.

    METHODS get_reservations
      RETURNING VALUE(reservations) TYPE t_zmmt0087.

    METHODS prepare_to_print
      RETURNING VALUE(fichas) TYPE ty_t_fichas.

*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Início de Alteração
    METHODS cadastrar_img_polegares
      IMPORTING
        i_t_outtab_items TYPE ty_t_saida.
*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Fim de Alteração

    METHODS set_title
      IMPORTING dynnr TYPE sy-dynnr.

    METHODS set_status
      IMPORTING dynnr TYPE sy-dynnr.

    METHODS set_selected_delivery
      IMPORTING item TYPE ty_saida.

    METHODS get_selected_delivery
      RETURNING VALUE(item) TYPE ty_saida.

    METHODS get_outtab_fichas
      RETURNING VALUE(items) TYPE ty_t_saida.

    METHODS user_command
      IMPORTING
        ucomm TYPE sy-ucomm
        dynnr TYPE sy-dynnr.

    METHODS display_photo.
    METHODS display.

    METHODS set_hierarchy
      CHANGING alv_tree TYPE REF TO cl_gui_alv_tree.

    METHODS set_events.
    METHODS set_toolbar.

    METHODS get_fieldcatalog
      RETURNING VALUE(fcat) TYPE lvc_t_fcat.

    METHODS call_return_popup
      RAISING cx_abap_util_exception.

    METHODS save_returned_data
      CHANGING
        main_delivery TYPE ty_saida.

    METHODS get_next_return_count
      IMPORTING
        reserva      TYPE zmmt0087-rsnum
        item         TYPE zmmt0087-rspos
        material     TYPE zmmt0087-matnr
        documento    TYPE zmmt0087-mblnr
      RETURNING
        VALUE(count) TYPE zmmt0087-count_devolucao.

    METHODS print
      IMPORTING
        matricula TYPE pa0465-pernr
        fichas    TYPE ty_t_fichas.

    METHODS get_ca_number
      IMPORTING
        material     TYPE mara-matnr
      RETURNING
        VALUE(value) TYPE bapi1003_alloc_values_char-value_char.

    METHODS get_material_description
      IMPORTING
        material     TYPE mara-matnr
      RETURNING
        VALUE(value) TYPE makt-maktx.

    METHODS handle_button_selected FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING
        fcode.

  PRIVATE SECTION.
    DATA reservations         TYPE TABLE OF zmmt0087.
    DATA reversemov           TYPE TABLE OF mseg.
    DATA outtab_fichas_epi    TYPE TABLE OF ty_saida.
    DATA reservations_header  TYPE TABLE OF rkpf.
    DATA reservation_items    TYPE TABLE OF resb.

    DATA alv_grid TYPE REF TO cl_gui_alv_grid.
    DATA custom   TYPE REF TO cl_gui_custom_container.

    DATA alv_tree TYPE REF TO cl_gui_alv_tree.
    DATA selected_delivery TYPE ty_saida.
*    DATA DOCKING  TYPE REF TO CL_GUI_DOCKING_CONTAINER.
    DATA toolbar  TYPE REF TO cl_gui_toolbar.
ENDCLASS.

*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Início de Alteração
CLASS zcx_erro_proc_imagem DEFINITION
  INHERITING FROM cx_static_check.
ENDCLASS.

CLASS zcx_erro_proc_imagem IMPLEMENTATION.
ENDCLASS.

CLASS zcl_cadastro_polegar DEFINITION .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_matricula TYPE zmmt0088-matricula
          iv_data      TYPE zmmt0088-dt_val_de,

      processar_polegares
        RAISING
          zcx_erro_proc_imagem,

      processar_polegar
        IMPORTING
          iv_imagem_jpeg TYPE xstring
          iv_polegar     TYPE string
        RAISING
          zcx_erro_proc_imagem.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_zmmt0088,
        matricula           TYPE zmmt0088-matricula,
        dt_val_de           TYPE zmmt0088-dt_val_de,
        im_polegar_esquerdo TYPE zmmt0088-im_polegar_esquerdo,
        im_polegar_direito  TYPE zmmt0088-im_polegar_direito,
      END OF ty_zmmt0088.


    DATA:
      mo_image_helper TYPE REF TO zcl_image_helper,
      wa_zmmt0088     TYPE ty_zmmt0088,
      v_data(8).

    METHODS:

      obter_dados
        IMPORTING
          iv_matricula   TYPE zmmt0088-matricula
          iv_data        TYPE zmmt0088-dt_val_de
        RETURNING
          VALUE(rv_data) TYPE dats,

      construir_nome_imagem
        IMPORTING
          iv_polegar     TYPE string
        RETURNING
          VALUE(rv_nome) TYPE tdobname,

      salva_imagem_bds
        IMPORTING
          iv_nome_imagem TYPE tdobname
          iv_imagem_jpeg TYPE xstring
        RAISING
          zcx_erro_proc_imagem .

ENDCLASS.

CLASS zcl_cadastro_polegar IMPLEMENTATION.

  METHOD constructor.

    me->obter_dados(
        iv_matricula   = iv_matricula
        iv_data        = iv_data
      ).

  ENDMETHOD.

  METHOD construir_nome_imagem.

    rv_nome = |{ iv_polegar }_{ me->wa_zmmt0088-matricula }{ me->v_data }|.

  ENDMETHOD.

  METHOD obter_dados.

    CLEAR me->v_data.

    SELECT SINGLE matricula
                  dt_val_de
                  im_polegar_esquerdo
                  im_polegar_direito
      FROM zmmt0088
      INTO me->wa_zmmt0088
      WHERE matricula = iv_matricula
      AND   dt_val_de <= iv_data
      AND   dt_val_ate >= iv_data.

    IF sy-subrc <> 0.

      SELECT SINGLE matricula
                  dt_val_de
                  im_polegar_esquerdo
                  im_polegar_direito
        FROM zmmt0088
        INTO me->wa_zmmt0088
        WHERE matricula = iv_matricula.

    ENDIF.

    IF sy-subrc = 0.

      IF  wa_zmmt0088-dt_val_de <> '19000101'
      AND wa_zmmt0088-dt_val_de IS NOT INITIAL.

        me->v_data = wa_zmmt0088-dt_val_de.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD processar_polegares.

    processar_polegar(
      EXPORTING
        iv_imagem_jpeg       = wa_zmmt0088-im_polegar_esquerdo
        iv_polegar           = 'DIGITAL_LEFT'
    ).

    processar_polegar(
      EXPORTING
        iv_imagem_jpeg       = wa_zmmt0088-im_polegar_direito
        iv_polegar           = 'DIGITAL_RIGHT'
   ).

  ENDMETHOD.

  METHOD processar_polegar.

    DATA: BEGIN OF l_bmptimestamp,
            lastdate TYPE d,
            lasttime TYPE t,
          END OF l_bmptimestamp.

    DATA(lv_nome_imagem) = me->construir_nome_imagem(
      iv_polegar        = iv_polegar
    ).

    CALL FUNCTION 'SAPSCRIPT_ATTRIB_GRAPHIC_BDS'
      EXPORTING
        i_object  = 'GRAPHICS'
        i_name    = lv_nome_imagem
        i_id      = 'BMAP'
        i_btype   = 'BCOL'
      IMPORTING
        e_ldate   = l_bmptimestamp-lastdate
        e_ltime   = l_bmptimestamp-lasttime
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 1.
      me->salva_imagem_bds(
        iv_nome_imagem = lv_nome_imagem
        iv_imagem_jpeg = iv_imagem_jpeg
      ).
    ELSEIF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_erro_proc_imagem.
    ENDIF.

  ENDMETHOD.

  METHOD salva_imagem_bds.

    IF mo_image_helper IS NOT BOUND.
      mo_image_helper = NEW zcl_image_helper( ).
    ENDIF.

    TRY.
        DATA(lv_bitmap_digital) = mo_image_helper->convert_jpeg_to_bmp( image_bin_str = iv_imagem_jpeg ).
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_erro_proc_imagem.
    ENDTRY.

    IF lv_bitmap_digital IS INITIAL.
      RAISE EXCEPTION TYPE zcx_erro_proc_imagem.
    ENDIF.

    mo_image_helper->save_bitmap_bds(
      EXPORTING
        name  = iv_nome_imagem
      CHANGING
        image = lv_bitmap_digital
    ).

  ENDMETHOD.

ENDCLASS.
*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Fim de Alteração


CLASS cl_main IMPLEMENTATION.
  METHOD run.
    me->select_reservations( ).
    CALL SCREEN 0001.
  ENDMETHOD.

  METHOD select_reservations.

    DATA: wa_reservations TYPE zmmt0087,
          wa_reversemov   TYPE mseg,
          lv_tabix        TYPE sy-tabix.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Preparando impressão...'.

    SELECT *
      FROM zmmt0087 AS a
INNER JOIN resb AS b ON b~rsnum = a~rsnum AND a~rspos = b~rspos
INNER JOIN rkpf AS c ON c~rsnum = a~rsnum
INNER JOIN zmmt0081 AS d ON d~rsnum = a~rsnum
INTO CORRESPONDING FIELDS OF TABLE me->reservations
     WHERE a~rsnum IN p_rsnum
       AND a~pernr IN p_pernr
       AND a~data  IN p_date
       AND b~werks IN p_werks
       AND b~matnr IN p_matnr
       AND b~xwaok EQ abap_true
*       AND B~XLOEK EQ ABAP_FALSE
       AND c~kostl IN p_kostl.
*>>> Begin of AYN -  CS0966578
*      AND NOT EXISTS ( SELECT *
*                       FROM MSEG WHERE A~MBLNR = MSEG~SMBLN ).
    IF sy-subrc EQ 0.

      SELECT *
        FROM mseg
        INTO CORRESPONDING FIELDS OF TABLE me->reversemov
         FOR ALL ENTRIES IN me->reservations
       WHERE smbln EQ me->reservations-mblnr
         AND rsnum EQ me->reservations-rsnum
         AND rspos EQ me->reservations-rspos.

      IF sy-subrc EQ 0.

        SORT me->reversemov BY smbln rsnum rspos.

        LOOP AT me->reservations INTO wa_reservations.
          MOVE sy-tabix TO lv_tabix.
          READ TABLE me->reversemov INTO wa_reversemov WITH KEY smbln = wa_reservations-mblnr
                                                                rsnum = wa_reservations-rsnum
                                                                rspos = wa_reservations-rspos.
          IF sy-subrc EQ 0.
            DELETE me->reservations INDEX lv_tabix.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

*<<< End of AYN - CS0966578.
    IF me->reservations IS INITIAL.
      MESSAGE 'Nenhuma ficha foi encontrada p/ os parâmetros informados.' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD get_reservations.
    MOVE me->reservations TO reservations.
  ENDMETHOD.

*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Início de Alteração
  METHOD cadastrar_img_polegares.

    DATA:
          lo_cad_polegar TYPE REF TO zcl_cadastro_polegar.


    DATA(lt_colaborador) = i_t_outtab_items.

    SORT lt_colaborador BY pernr data.
    DELETE ADJACENT DUPLICATES FROM lt_colaborador COMPARING pernr data.


    LOOP AT lt_colaborador INTO DATA(ls_colaborador).

      CLEAR lo_cad_polegar.

      TRY.
          CREATE OBJECT lo_cad_polegar
            EXPORTING
              iv_matricula = ls_colaborador-pernr
              iv_data      = ls_colaborador-data.

          lo_cad_polegar->processar_polegares( ).

        CATCH cx_root.
          "Erro ao executar a classe...
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Fim de Alteração

  METHOD prepare_to_print.
    DATA: BEGIN OF l_bmptimestamp,
            lastdate TYPE d,
            lasttime TYPE t,
          END OF l_bmptimestamp.


    DATA ficha_epi TYPE zmme_ficha_controle_epi.
    DATA: wa_zmmt0088 TYPE zmmt0088.
    DATA: wa_zmmt0081 TYPE zmmt0081.
    DATA  im_digital  TYPE zmme_ficha_controle_epi-user_digital_left.
    DATA v_data(8).

    DATA binary_data  TYPE xstring.
    DATA base64_image TYPE string.
    DATA str_table    TYPE STANDARD TABLE OF x255.
    "
    DATA i_object TYPE  stxbitmaps-tdobject.
    DATA i_name   TYPE  stxbitmaps-tdname.
    DATA i_id     TYPE  stxbitmaps-tdid.
    DATA i_btype  TYPE  stxbitmaps-tdbtype.
    "
    DATA(_outtab_items) = me->get_outtab_fichas( ).
    SORT _outtab_items BY rsnum rspos matnr mblnr count_devolucao.

    cl_progress_indicator=>progress_indicate(
      i_text = 'Preparando impressão da ficha de EPI...'
    ).

*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Início de Alteração
    me->cadastrar_img_polegares( i_t_outtab_items = _outtab_items ).
*** Stefanini - IR257353 - 22/09/2025 - LAZAROSR - Fim de Alteração

    LOOP AT _outtab_items INTO DATA(_outtab_item).

      CLEAR ficha_epi.
      "
      ficha_epi-codigo_epi    = _outtab_item-matnr.
      ficha_epi-descricao_epi = |{ CONV i( _outtab_item-matnr ) } - { _outtab_item-maktx }|.
      ficha_epi-numero_ca     = _outtab_item-numero_ca.

      CLEAR wa_zmmt0081.
      SELECT SINGLE *
        FROM zmmt0087
        INTO @DATA(wa_zmmt0087)
         WHERE rsnum  EQ @_outtab_item-rsnum
         AND   operacao EQ  @_outtab_item-operacao.

      SELECT SINGLE *
        FROM zmmt0081
        INTO wa_zmmt0081
         WHERE rsnum  EQ _outtab_item-rsnum.

      IF sy-subrc = 0 AND wa_zmmt0081-lado IS NOT INITIAL.
        IF wa_zmmt0081-im_polegar IS NOT INITIAL.
          im_digital = |DIGITAL_RES_{ _outtab_item-operacao } { _outtab_item-rsnum }|.
          IF _outtab_item-lado_log = 'L'.
            ficha_epi-user_digital_left  = im_digital.
            ficha_epi-user_digital_right = 'X'.
          ELSE.
            ficha_epi-user_digital_left  = 'X'.
            ficha_epi-user_digital_right = im_digital.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR v_data.
        SELECT SINGLE *
          FROM zmmt0088
          INTO wa_zmmt0088
           WHERE matricula  EQ _outtab_item-pernr
           AND   dt_val_de  LE _outtab_item-data
           AND   dt_val_ate GE _outtab_item-data.

        IF sy-subrc NE 0.
          SELECT SINGLE *
              FROM zmmt0088
              INTO wa_zmmt0088
               WHERE matricula  EQ _outtab_item-pernr.
        ENDIF.

        IF sy-subrc EQ 0.
          IF wa_zmmt0088-dt_val_de NE  '19000101' AND wa_zmmt0088-dt_val_de IS NOT INITIAL. "Cadastro inicial
            v_data = wa_zmmt0088-dt_val_de.
          ENDIF.
        ENDIF.
      ENDIF.
      IF ( ficha_epi-user_digital_left IS NOT INITIAL OR ficha_epi-user_digital_right IS NOT INITIAL )  AND wa_zmmt0087-senha IS INITIAL.
        IF wa_zmmt0081-lado IS INITIAL.
          ficha_epi-user_digital_left  = |DIGITAL_LEFT_{ _outtab_item-pernr }{ v_data }|.
          im_digital = ficha_epi-user_digital_left.
          ficha_epi-user_digital_right = |DIGITAL_RIGHT_{ _outtab_item-pernr }{ v_data }|.
        ENDIF.

        i_object  = 'GRAPHICS'.
        i_name    = im_digital.
        i_id      = 'BMAP'.
        i_btype   = 'BCOL'.

        "Verifica se existe a imagem / se não existir grava uma nova imagem no servidor
        CALL FUNCTION 'SAPSCRIPT_ATTRIB_GRAPHIC_BDS'
          EXPORTING
            i_object  = i_object
            i_name    = i_name
            i_id      = i_id
            i_btype   = i_btype
          IMPORTING
            e_ldate   = l_bmptimestamp-lastdate
            e_ltime   = l_bmptimestamp-lasttime
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.

        IF sy-subrc = 1.
          DATA(_image_helper) = NEW zcl_image_helper( ).
          IF wa_zmmt0081-lado IS INITIAL.
            binary_data = wa_zmmt0088-im_polegar_esquerdo.
          ELSE.
            IF _outtab_item-im_polegar_log IS NOT INITIAL.
              binary_data = _outtab_item-im_polegar_log.
            ELSE.
              binary_data = wa_zmmt0081-im_polegar.
            ENDIF.
          ENDIF.
          "//Call JPEG->BMP transformation
          DATA(_bitmap_left_digital) = _image_helper->convert_jpeg_to_bmp( image_bin_str = binary_data ).


          _image_helper->save_bitmap_bds(
            EXPORTING
              name   = im_digital
            CHANGING
              image  = _bitmap_left_digital
          ).

          IF wa_zmmt0081-lado IS INITIAL.
            "//Call JPEG->BMP transformation
            binary_data = wa_zmmt0088-im_polegar_direito.
            "
            DATA(_bitmap_right_digital) = _image_helper->convert_jpeg_to_bmp( image_bin_str = binary_data ).

            _image_helper->save_bitmap_bds(
              EXPORTING
                name   = |DIGITAL_RIGHT_{ wa_zmmt0088-matricula }{ v_data }|
              CHANGING
                image  = _bitmap_right_digital
            ).
          ENDIF.
        ENDIF.
      ELSE. "SENHA
        ficha_epi-user_digital_left  = 'MAGGI_SENHA'.
        ficha_epi-user_digital_right = 'X'.
      ENDIF.
      "
      SELECT SINGLE bwart
        FROM rkpf
        INTO @DATA(_bwart)
      WHERE rsnum = @_outtab_item-rsnum.

      IF _bwart = '202'. " Devolução ao estoque Ja feitos
        _outtab_item-operacao = 'D'.
      ENDIF.

      CASE _outtab_item-operacao.
        WHEN 'E'. "/Entrega
          ficha_epi-qtd_retirada   = _outtab_item-quant.
          ficha_epi-data_entrega   = _outtab_item-data.
        WHEN 'D'. "/Devolução
          ficha_epi-qtd_devolvida  = _outtab_item-quant.
          ficha_epi-data_devolucao = _outtab_item-data.
        WHEN OTHERS.
      ENDCASE.

      APPEND ficha_epi TO fichas.
      CLEAR ficha_epi.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_title.
    CASE dynnr.
      WHEN 0001.
        zcl_hrst_commons=>get_employee(
          EXPORTING
            matricula      = p_pernr-low
          RECEIVING
            employee       = employee
          EXCEPTIONS
            data_not_found = 1
            OTHERS         = 2
        ).

        SET TITLEBAR '0001' WITH employee-nome.

      WHEN 0002.
        DATA(_material) = |{ CONV i( me->get_selected_delivery( )-matnr ) } - { me->get_selected_delivery( )-maktx }|.
        SET TITLEBAR '0002' WITH _material.
    ENDCASE.
  ENDMETHOD.

  METHOD set_status.
    IF ( dynnr = 0001 ).
      SET PF-STATUS '0001'.
    ELSE.
      SET PF-STATUS '0002'.
    ENDIF.
  ENDMETHOD.

  METHOD set_selected_delivery.
    MOVE item TO me->selected_delivery.
  ENDMETHOD.

  METHOD get_selected_delivery.
    MOVE me->selected_delivery TO item.
  ENDMETHOD.

  METHOD get_outtab_fichas.
    MOVE me->outtab_fichas_epi TO items.
  ENDMETHOD.

  METHOD user_command.
    CASE dynnr.

      WHEN 0001. "//Main screen
        CASE ucomm  .
          WHEN 'EXIT' OR 'CANCEL'.
            LEAVE PROGRAM.
          WHEN 'BACK'.
            LEAVE TO SCREEN 0.
          WHEN OTHERS.
        ENDCASE.

      WHEN 0002. "//Popup to return
        CASE ucomm  .
          WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
            LEAVE TO SCREEN 0.

          WHEN 'OK'.
            DATA(_selected_delivery) = me->get_selected_delivery( ).
            IF senha_to_return IS INITIAL.
              TRY.
                  zcl_biometry=>read_digital(
                    EXPORTING
                      registration = employee-matricula
                    RECEIVING
                      result       = DATA(_result)
                  ).
                CATCH zcx_biometry.
              ENDTRY.
            ELSE.
              zcl_biometry=>read_password(
                  EXPORTING
                    registration =  employee-matricula
                  RECEIVING
                    result       = DATA(_results2) ).

              IF _results2-senha NE senha_to_return.
                CLEAR senha_to_return.
                MESSAGE TEXT-001 TYPE 'I' DISPLAY LIKE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            IF quantity_to_return <= ( _selected_delivery-quant - _selected_delivery-returned_qty ).

              TRY.
                  IF senha_to_return IS INITIAL.
                    CALL METHOD zcl_biometry=>validate_digital
                      EXPORTING
                        hash_validation_l = _result-polegar_esquerdo
                        hash_validation_r = _result-polegar_direito
                      RECEIVING
                        result            = DATA(_zmmt0081).
                    _selected_delivery-lado_log       =  _zmmt0081-lado.
                    _selected_delivery-polegar_log    =  _zmmt0081-polegar.
                    _selected_delivery-im_polegar_log =  _zmmt0081-im_polegar.
                  ENDIF.
                  "
                  me->save_returned_data( CHANGING main_delivery = _selected_delivery ).

                  MESSAGE 'Material devolvido com sucesso!' TYPE 'S'.
                  LEAVE TO SCREEN 0.

                CATCH zcx_biometry INTO DATA(_cx).
                  MESSAGE _cx->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
              ENDTRY.

            ELSE.
              MESSAGE |A quantidade informada é superior a quantidade à devolver ({ _selected_delivery-quant - _selected_delivery-returned_qty }).|
                 TYPE 'I' DISPLAY LIKE 'E'.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD display_photo.
    DATA(_image_helper) = NEW zcl_image_helper( ).

    DATA(_url) = _image_helper->create_internal_url( employee-foto_base64 ).

    _image_helper->display(
      EXPORTING
        custom_name      = 'CUSTOM_PHOTO'
        url              = _url
      CHANGING
        custom_instance  = custom_photo
        picture_instance = picture
    ).
  ENDMETHOD.

  METHOD display.
    IF me->custom IS INITIAL.

      CREATE OBJECT me->custom
        EXPORTING
          container_name = 'CUSTOM_MAIN'.

      IF me->alv_tree IS INITIAL.

        CREATE OBJECT me->alv_tree
          EXPORTING
            parent              = custom
            node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
            item_selection      = abap_true
            no_html_header      = abap_true
            no_toolbar          = abap_false.

        "// Get Field Catalog

        "ZCL_UTIL=>GET_STRUCTURE_DESCRIPTION( 'ZPPT0008' ).

        DATA(fieldcat) = me->get_fieldcatalog( ).
        DATA(header) = VALUE treev_hhdr( LET x = 'Reserva/Item' IN heading = x tooltip = x width   = 28 ).

        "// Set Tree Display
        CALL METHOD me->alv_tree->set_table_for_first_display
          EXPORTING
            is_hierarchy_header = header
          CHANGING
            it_outtab           = outtab_fichas_epi
            it_fieldcatalog     = fieldcat.

        "// Set Ov Hierarchy
        me->set_hierarchy( CHANGING alv_tree = me->alv_tree ).

        me->set_toolbar( ).
        me->set_events( ).
      ENDIF.
    ENDIF.

    cl_gui_cfw=>flush( ).
    me->alv_tree->frontend_update( ).
  ENDMETHOD.

  METHOD set_hierarchy.
    DATA ficha_epi TYPE ty_saida.
    DATA layout    TYPE lvc_s_layn.

    DATA(_reservas_baixadas) = me->get_reservations( ).

    SORT _reservas_baixadas BY rsnum rspos matnr mblnr count_devolucao.
    LOOP AT _reservas_baixadas INTO DATA(_reserva).

      MOVE-CORRESPONDING _reserva TO ficha_epi.

      ficha_epi-numero_ca = me->get_ca_number( _reserva-matnr ).
      ficha_epi-maktx     = me->get_material_description( _reserva-matnr ).

*                LAYOUT_NODE =
*            VALUE LVC_S_LAYN( ISFOLDER  = ABAP_FALSE
*                              N_IMAGE   = ICON_ORDER
*                              EXP_IMAGE = ICON_ORDER
*                              STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED
*                            ).

      IF ( _reserva-count_devolucao IS INITIAL ).

        SELECT SUM( quant )
          FROM zmmt0087
          INTO ficha_epi-returned_qty
         WHERE rsnum = ficha_epi-rsnum
           AND rspos = ficha_epi-rspos
           AND matnr = ficha_epi-matnr
           AND mblnr = ficha_epi-mblnr
           AND operacao = 'D'.

        ficha_epi-quant_to_return = ficha_epi-quant - ficha_epi-returned_qty.

        layout = VALUE lvc_s_layn( isfolder  = abap_false
                                   n_image   = icon_outgoing_employee
                                   exp_image = icon_outgoing_employee
                                   style     = cl_gui_column_tree=>style_emphasized
                                 ).

        CALL METHOD alv_tree->add_node
          EXPORTING
            i_relat_node_key = space
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = CONV #( _reserva-rsnum )
            is_node_layout   = layout
            is_outtab_line   = ficha_epi
            it_item_layout   = VALUE #( ( fieldname = 'QUANT_TO_RETURN' style = cl_gui_column_tree=>style_intensified t_image = COND #( WHEN ficha_epi-quant_to_return IS INITIAL THEN icon_led_green ELSE icon_led_yellow ) ) )
          IMPORTING
            e_new_node_key   = DATA(item_key).

      ELSE.

        layout = VALUE lvc_s_layn( isfolder  = abap_false
                                   n_image   = icon_incoming_employee
                                   exp_image = icon_incoming_employee
*                                   STYLE     = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED
                                 ).

        CALL METHOD alv_tree->add_node
          EXPORTING
            i_relat_node_key = item_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = CONV #( _reserva-count_devolucao )
            is_node_layout   = layout
            it_item_layout   = VALUE #( ( fieldname = 'QUANT_TO_RETURN' hidden = abap_true ) )
            is_outtab_line   = ficha_epi.
      ENDIF.

      CLEAR ficha_epi.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_events.
    DATA lt_events TYPE cntl_simple_events.

    me->alv_tree->get_registered_events(
      IMPORTING
        events     = lt_events ).

    CALL METHOD me->alv_tree->set_registered_events
      EXPORTING
        events = lt_events.

    SET HANDLER me->handle_button_selected   FOR me->toolbar.
  ENDMETHOD.

  METHOD set_toolbar.
    CALL METHOD me->alv_tree->get_toolbar_object
      IMPORTING
        er_toolbar = me->toolbar.

    CHECK NOT me->toolbar IS INITIAL.

    "//Modify toolbar with methods of CL_GUI_TOOLBAR:
    "//add seperator to toolbar
    CALL METHOD me->toolbar->add_button
      EXPORTING
        fcode     = ''
        icon      = ''
        butn_type = cntb_btype_sep.

    CALL METHOD me->toolbar->add_button
      EXPORTING
        fcode     = 'DEVOLVER'
        icon      = icon_incoming_employee
        butn_type = cntb_btype_button
        text      = 'Devolver'
        quickinfo = 'Devolver'.

    CALL METHOD me->toolbar->add_button
      EXPORTING
        fcode     = 'IMPRIMIR_FICHA'
        icon      = icon_print
        butn_type = cntb_btype_button
        text      = 'Imprimir Ficha'
        quickinfo = 'Imprimir Ficha'.
  ENDMETHOD.

  METHOD get_fieldcatalog.
    fcat = VALUE #(
      ( fieldname = 'MATNR'     coltext = 'Cód. EPI'        outputlen = 15 no_zero = abap_true )
      ( fieldname = 'MAKTX'     coltext = 'Descrição'       outputlen = 40                     )
      ( fieldname = 'NUMERO_CA' coltext = 'Número CA'       outputlen = 15                     )
      ( fieldname = 'DATA'      coltext = 'Data E/D'        outputlen = 15 ref_table = 'ZMMT0087' ref_field = 'DATA' )
      ( fieldname = 'QUANT'     coltext = 'Quantidade E/D'  outputlen = 20 just = 'C'          )
      ( fieldname = 'QUANT_TO_RETURN' coltext = 'Qtd. à devolver' outputlen = 20                              )
    ).
  ENDMETHOD.

  METHOD call_return_popup.
    CALL SCREEN 0002 STARTING AT 35 10 ENDING AT 90 10.

    IF sy-ucomm = 'BACK' OR sy-ucomm = 'EXIT'.
      RAISE EXCEPTION TYPE cx_abap_util_exception.
    ENDIF.

    CLEAR quantity_to_return.
  ENDMETHOD.

  METHOD save_returned_data.
    DATA delivery_children TYPE ty_saida.

    zcl_biometry=>get_system_data(
      IMPORTING
        username      = DATA(_logged_user)
        computer_name = DATA(_computer_name)
        ip_adress     = DATA(_ip)
    ).

    "//Insert new item;
    CALL METHOD me->get_next_return_count
      EXPORTING
        reserva   = main_delivery-rsnum
        item      = main_delivery-rspos
        material  = main_delivery-matnr
        documento = main_delivery-mblnr
      RECEIVING
        count     = DATA(_count_devolucao).

    DATA(_item) =
      VALUE zmmt0087( rsnum           = main_delivery-rsnum
                      rspos           = main_delivery-rspos
                      matnr           = main_delivery-matnr
                      mblnr           = main_delivery-mblnr
                      operacao        = 'D' "//Devolução
                      count_devolucao = _count_devolucao
                      quant           = quantity_to_return
                      pernr           = main_delivery-pernr
                      hash_validation = ''
                      lado_log        = main_delivery-lado_log
                      polegar_log     = main_delivery-polegar_log
                      im_polegar_log  = main_delivery-im_polegar_log
                      data            = sy-datum
                      hora            = sy-uzeit
                      operador        = sy-uname
                      nome_computador = _computer_name
                      usuario_logado  = _logged_user
                      ip              = _ip
                      senha           = senha_to_return
                    ).

    INSERT zmmt0087 FROM _item.
    COMMIT WORK.

    "//Insert children delivery
    MOVE-CORRESPONDING main_delivery TO delivery_children.

    delivery_children-quant           = quantity_to_return.
    delivery_children-count_devolucao = _count_devolucao.
    delivery_children-operacao        = 'D'.

    CALL METHOD me->alv_tree->add_node
      EXPORTING
        i_relat_node_key = main_delivery-node
        i_relationship   = cl_gui_column_tree=>relat_last_child
        is_outtab_line   = delivery_children
        is_node_layout   = VALUE lvc_s_layn( isfolder = abap_false n_image = icon_incoming_employee exp_image = icon_incoming_employee )
        i_node_text      = CONV #( _item-count_devolucao ).

    "//Change main delivery
    main_delivery-returned_qty    = main_delivery-returned_qty + quantity_to_return.
    main_delivery-quant_to_return = main_delivery-quant - main_delivery-returned_qty.

    CALL METHOD me->alv_tree->change_node
      EXPORTING
        i_node_key     = main_delivery-node
        i_outtab_line  = main_delivery
        it_item_layout = VALUE #( ( fieldname = 'QUANT_TO_RETURN'
                                    style     = cl_gui_column_tree=>style_intensified
                                    t_image   = COND #( WHEN main_delivery-quant_to_return IS INITIAL THEN icon_led_green ELSE icon_led_yellow ) ) ).

    me->alv_tree->expand_node( i_node_key = main_delivery-node ).
    me->alv_tree->update_calculations( ).
    me->alv_tree->frontend_update( ).
  ENDMETHOD.

  METHOD get_ca_number.
    DATA valuesnum  TYPE TABLE OF bapi1003_alloc_values_num.
    DATA valueschar TYPE TABLE OF bapi1003_alloc_values_char.
    DATA valuescurr TYPE TABLE OF bapi1003_alloc_values_curr.
    DATA return     TYPE TABLE OF  bapiret2.

    "*---> 05/07/2023 - Migração S4 - LO
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        objectkey       = CONV objnum( material )
        objecttable     = 'MARA'
        classnum        = 'MATEPI'
        classtype       = '023'
      TABLES
        allocvaluesnum  = valuesnum
        allocvalueschar = valueschar
        allocvaluescurr = valuescurr
        return          = return.

    TRY.
        value = valueschar[ charact = 'ZEPI_CA' ]-value_char.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD get_next_return_count.
    SELECT MAX( count_devolucao )
      FROM zmmt0087
      INTO count
     WHERE rsnum = reserva
       AND rspos = item
       AND matnr = material
       AND mblnr = documento
       AND operacao = 'D'.

    ADD 1 TO count.
  ENDMETHOD.

  METHOD get_material_description.
    SELECT SINGLE maktx
      FROM makt
      INTO value
     WHERE matnr = material.
  ENDMETHOD.

  METHOD handle_button_selected.
    CASE fcode.
      WHEN 'DEVOLVER'.
        DATA outtab_main_item     TYPE ty_saida.
        DATA outtab_children_item TYPE ty_saida.
        DATA selected_nodes       TYPE lvc_t_nkey.
        DATA selected_node        TYPE lvc_nkey.

        me->alv_tree->get_selected_nodes( CHANGING ct_selected_nodes = selected_nodes ).

        IF ( selected_nodes IS INITIAL ).
          me->alv_tree->get_selected_item( IMPORTING e_selected_node = selected_node ).
        ELSE.
          selected_node = selected_nodes[ 1 ].
        ENDIF.

        IF selected_node IS INITIAL.
          MESSAGE 'Selecione a linha da entrega p/ realizar a devolução' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CALL METHOD me->alv_tree->get_outtab_line
          EXPORTING
            i_node_key     = selected_node
          IMPORTING
            e_outtab_line  = outtab_main_item
            es_node_layout = DATA(_layout).

        IF ( _layout-n_image = icon_outgoing_employee ).

          CALL METHOD me->alv_tree->get_children
            EXPORTING
              i_node_key  = selected_node
            IMPORTING
              et_children = DATA(_childrens).

          outtab_main_item-node = selected_node.

          IF ( outtab_main_item-returned_qty < outtab_main_item-quant ).
            me->set_selected_delivery( outtab_main_item ).

            TRY.
                me->call_return_popup( ).
              CATCH cx_abap_util_exception.
            ENDTRY.
          ENDIF.

        ELSE.
          MESSAGE 'Seleciona a linha da entrega p/ realizar a devolução.' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'IMPRIMIR_FICHA'.
        CALL METHOD me->print
          EXPORTING
            matricula = employee-matricula
            fichas    = me->prepare_to_print( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD print.
    DATA form_name TYPE rs38l_fnam.

    DATA i_object TYPE  stxbitmaps-tdobject.
    DATA i_name   TYPE  stxbitmaps-tdname.
    DATA i_id     TYPE  stxbitmaps-tdid.
    DATA i_btype  TYPE  stxbitmaps-tdbtype.

    DATA(_options_ordem) =
      VALUE ssfcompop( tddest    = 'LOCL'
                       tdnoprint = abap_false
                       tdimmed   = abap_true
                       tdnewid   = abap_true
                       tdnoarch  = abap_true ).

    DATA(_control_ordem) =
      VALUE ssfctrlop( device    = 'PRINTER'
                       preview   = abap_true
                       no_dialog = abap_false ).

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'ZMMSF_FICHA_CONTROLE_EPI'
      IMPORTING
        fm_name  = form_name.

    CALL FUNCTION form_name
      EXPORTING
        control_parameters = _control_ordem
        output_options     = _options_ordem
        user_settings      = abap_true
        matricula          = matricula
      TABLES
        items_epi          = fichas
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    i_object  = 'GRAPHICS'.
    i_name    = ''.
    i_id      = 'BMAP'.
    i_btype   = 'BCOL'.

    "APAGA IMAGENS
    LOOP AT fichas INTO DATA(_ficha).
      IF  ( _ficha-user_digital_left = 'X' OR _ficha-user_digital_right = 'X' ) AND _ficha-user_digital_left  NE 'MAGGI_SENHA'.
        IF _ficha-user_digital_left NE 'X'.
          i_name = _ficha-user_digital_left.
        ELSE.
          i_name = _ficha-user_digital_right.
        ENDIF.
        CALL FUNCTION 'SAPSCRIPT_DELETE_GRAPHIC_BDS' "
          EXPORTING
            i_object       = i_object
            i_name         = i_name
            i_id           = i_id
            i_btype        = i_btype
            dialog         = ' '
          EXCEPTIONS
            enqueue_failed = 1          "
            delete_failed  = 2           "
            not_found      = 3               "
            canceled       = 4.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

DATA main TYPE REF TO cl_main.

START-OF-SELECTION.
  main = NEW cl_main( ).
  main->run( ).
*&---------------------------------------------------------------------*
*&      Module  MAIN_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pbo OUTPUT.

  main->set_title( sy-dynnr ).
  main->set_status( sy-dynnr ).

  CASE sy-dynnr.
    WHEN 0002.
      SELECT SINGLE *
        FROM zmmt0120
        INTO @DATA(_zmmt0120)
       WHERE matricula  EQ @employee-matricula.
      IF sy-subrc = 0. "Mostra Senha.
        LOOP AT SCREEN.
          IF screen-name = 'TXTSENHA'.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.
          IF screen-name = 'SENHA_TO_RETURN'.
            screen-invisible = 1.
            screen-input = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        LOOP AT SCREEN.
          IF screen-name = 'TXTSENHA'.
            screen-invisible = 1.
            MODIFY SCREEN.
          ENDIF.
          IF screen-name = 'SENHA_TO_RETURN'.
            screen-invisible = 1.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN 0001.
      main->display( ).
      main->display_photo( ).
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MAIN_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE main_pai INPUT.
  main->user_command( ucomm = sy-ucomm dynnr = sy-dynnr ).
ENDMODULE.
