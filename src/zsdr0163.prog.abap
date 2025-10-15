*&---------------------------------------------------------------------*
*& Report ZSDR0163
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0163.

*----------------------------------------------------------------------*
* INCLUDE                                                              *
*----------------------------------------------------------------------*
INCLUDE zsdr0163_top.
INCLUDE zsdr0163_cla.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lv_tabela VALUE 'zarixsd1',
        lv_handle LIKE sy-tabix.


  DATA: lt_zarixsd1  TYPE TABLE OF zarixsd1,
        lt_vbrp_temp TYPE TABLE OF vbrp,
        lt_vbrp      TYPE TABLE OF vbrp.

  DATA ls_borident LIKE  borident.

  SELECT *
  FROM zarixsd1
  INTO TABLE lt_zarixsd1
  WHERE archivekey LIKE '%SD_VBRK'
    AND vbeln IN s_vbeln
    AND fkdat IN s_fkdat
    AND fkart IN s_fkart
    AND vkorg IN s_vkorg.


  LOOP AT lt_zarixsd1 INTO DATA(wa_zarixsd1).

    ls_borident-objkey = wa_zarixsd1-vbeln.

    CLEAR: lt_vbrp_temp[].

    CALL FUNCTION 'ASH_SD_VBRK_READ'
      EXPORTING
        i_borident             = ls_borident
      TABLES
        et_vbrp                = lt_vbrp_temp
      EXCEPTIONS
        not_in_infostructure   = 1
        not_in_archive         = 2
        no_instructure_defined = 3
        OTHERS                 = 4.
    IF sy-subrc = 0.
      APPEND LINES OF lt_vbrp_temp TO lt_vbrp[].
    ENDIF.

  ENDLOOP.

  DELETE lt_vbrp WHERE vgbel NOT IN s_vgbel.

  DATA: lr_refkey TYPE RANGE OF j_1brefkey.

  lr_refkey = VALUE #(  FOR ls_value IN lt_vbrp ( sign = 'I'
                                                  option = 'EQ'
                                                  low = ls_value-vbeln
                                                   ) ).
  CHECK lr_refkey[] IS NOT INITIAL.

  SELECT doc~candat, lin~refkey, lin~docnum, act~regio, act~nfyear, act~nfmonth, act~stcd1, act~model, act~serie, act~nfnum9, act~docnum9, act~cdv
      FROM j_1bnflin AS lin
    INNER JOIN j_1bnfdoc AS doc
    ON lin~docnum = doc~docnum
    INNER JOIN j_1bnfe_active AS act
    ON act~docnum = doc~docnum
    INTO TABLE @DATA(lt_nota_fiscal)
    WHERE refkey IN @lr_refkey.

  LOOP AT lt_nota_fiscal INTO DATA(wa_nfe).

    READ TABLE lt_vbrp WITH KEY vbeln = wa_nfe-refkey INTO DATA(wa_vbrp).
    IF sy-subrc <> 0.
      CLEAR wa_vbrp.
    ENDIF.

    APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

    <fs_data>-vgbel    = wa_vbrp-vgbel.
    <fs_data>-vbeln    = wa_nfe-refkey(10).
    <fs_data>-docnum   = wa_nfe-docnum.
    <fs_data>-candat   = wa_nfe-candat.

    CONCATENATE wa_nfe-regio
                wa_nfe-nfyear
                wa_nfe-nfmonth
                wa_nfe-stcd1
                wa_nfe-model
                wa_nfe-serie
                wa_nfe-nfnum9
                wa_nfe-docnum9
                wa_nfe-cdv INTO <fs_data>-chave_nfe.



  ENDLOOP.



*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF sy-sysid = 'DEV' OR
     sy-sysid = 'QAS'.

    APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<fs>).
    <fs>-vgbel     = '123456'.
    <fs>-vbeln     = '222222'.
    <fs>-docnum    = '151515'.
    <fs>-chave_nfe = '20241234567895623145'.

    APPEND INITIAL LINE TO gt_data ASSIGNING <fs>.
    <fs>-vgbel     = '222222'.
    <fs>-vbeln     = '333333'.
    <fs>-docnum    = '17171717'.
    <fs>-chave_nfe = '20241234567895623145'.

  ENDIF.

* Any data found?
  IF gt_data[] IS INITIAL.

    MESSAGE i037(qc).
    RETURN.

  ELSE.

    PERFORM display_data  CHANGING gt_data
                                   gr_alv.
  ENDIF.


************************************************************************
************************************************************************
*   F O R M S  F O R M S  F O R M S  F O R M S  F O R M S  F O R M S   *
************************************************************************
************************************************************************

FORM display_data CHANGING p_data TYPE tab_data
                          p_alv  TYPE REF TO cl_salv_table.

DATA:
      gr_table TYPE REF TO cl_salv_table,
      gr_functions TYPE REF TO cl_salv_functions,
      gr_columns TYPE REF TO cl_salv_columns,
      gr_layout  TYPE REF TO cl_salv_layout.

  " Criar uma instância da tabela ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = p_data ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao criar a tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar funções
  TRY.
      gr_functions = gr_table->get_functions( ).
      gr_functions->set_all( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar as funções da tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar colunas
  TRY.
      gr_columns = gr_table->get_columns( ).
      gr_columns->set_optimize( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar as colunas da tabela ALV' TYPE 'E'.
  ENDTRY.

  " Configurar layout
  TRY.
      gr_layout = gr_table->get_layout( ).
*      gr_layout->set_key( 'BACKGROUND_ALV' ).
      gr_layout->set_default( abap_true ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao configurar o layout da tabela ALV' TYPE 'E'.
  ENDTRY.

  " Exibir a tabela ALV
  TRY.
      gr_table->display( ).
    CATCH cx_salv_msg.
      " Tratar exceções de mensagem
      MESSAGE 'Erro ao exibir a tabela ALV' TYPE 'E'.
  ENDTRY.


ENDFORM.
