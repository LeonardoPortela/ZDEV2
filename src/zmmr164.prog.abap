*&---------------------------------------------------------------------*
*& Report  ZMMR164
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr164.
TABLES: rbkp.

"Tabela Interna Global
DATA: git_t001 TYPE TABLE OF t001.
DATA: git_sflight TYPE TABLE OF sflight.
DATA: git_filtro TYPE zif_screen_linha_filtro_t,
      t_zmme0002 TYPE TABLE OF zmme0002.


"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,
      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.



DATA: t_rbkp TYPE TABLE OF rbkp.


*&---------------------------------------------------------------------*
**Tela de Parâmetros 2
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_gjahr FOR rbkp-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY ,
                p_belnr FOR rbkp-belnr  OBLIGATORY .
SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
*  PERFORM fm_dados_processa.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  TYPES: BEGIN OF ty_pdf_miro.
  TYPES:   pdf_doc TYPE string.
  TYPES: END OF ty_pdf_miro.


  DATA: objeto         TYPE REF TO zcl_soft_expert_ws_inject,
        it_lista       TYPE TABLE OF bdn_con,
        lc_objkey      TYPE swotobjid-objkey,
        document_id    TYPE sofolenti1-doc_id,
        document_data  TYPE sofolenti1,
        wa_anexos      TYPE zde_zsexpt00007,
        t_anexos       TYPE TABLE OF zde_zsexpt00007,
        cont_hex       TYPE TABLE OF solix,
        l_data         TYPE TABLE OF w3mime,
        s_data         TYPE w3mime,
        t_document     TYPE TABLE OF sofolenti1,
        name_file      TYPE string,
        w_pdf          TYPE ty_pdf_miro,
        it_pdf         TYPE STANDARD TABLE OF ty_pdf_miro,
        v_bin_filesize TYPE i.


  DATA: id TYPE char3.
  DATA: miro TYPE srgbtbrel-instid_a.

  DATA: r_miro TYPE RANGE OF awkey.
  DATA: r_objkey TYPE RANGE OF awkey.
  "Selecionar dados da MIRO.

**Selecionado a miro valida.
  FREE: t_rbkp.
  SELECT *
  FROM rbkp
  INTO TABLE t_rbkp
  WHERE belnr IN p_belnr
    AND gjahr IN p_gjahr.

  r_miro = VALUE rsis_t_range( FOR l IN t_rbkp ( low = |{ l-belnr }{ p_gjahr-low }| option = 'EQ' sign = 'I' ) ).

*  "Selecionar documentos valido MIRO.
*  SELECT  *
*  FROM bkpf
*  INTO TABLE @DATA(t_bkpf)
*  WHERE awkey IN @r_miro.
*
*  r_objkey = VALUE rsis_t_range( FOR l IN t_rbkp ( low = |{ l-belnr }{ l-gjahr } | option = 'EQ' sign = 'I' ) ).

  "Buscando ID dos documentos anexa a cada MIRO.
  SELECT * FROM srgbtbrel INTO TABLE @DATA(t_srgbtbrel) WHERE instid_a IN @r_miro.

  IF sy-subrc EQ 0.
    LOOP AT t_srgbtbrel ASSIGNING FIELD-SYMBOL(<l_docum>).

      "Setando o ID do documento.
      document_id = <l_docum>-instid_b.

      "Condição para verificar se a MIRO anterio é igual a MIRO atual no LOOP, para definição do ID do ITEM do arquivo.
      IF <l_docum>-instid_a EQ miro.
        ADD 1 TO id.
      ELSE.
        id = 1.
      ENDIF.


      "Busca o detalhe do arquivo anexado a miro e PDF converte em Binario.
      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
        EXPORTING
          document_id                = document_id "ID do arquivo.
        IMPORTING
          document_data              = document_data "Retorno com detalhe do arquivo.
        TABLES
          contents_hex               = l_data "Retorno em Binario.
        EXCEPTIONS
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          OTHERS                     = 4.

      "Definição do local e nome do arquivo que sera gravado na unidade.
      CONCATENATE 'C:\MIRO\'<l_docum>-instid_a'_ITEM'id'.PDF' INTO name_file.

      "Setando a MIRO atual.
      miro = <l_docum>-instid_a.

      "Executando o download o arquivo no diretorio definido.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = name_file "Local onde sera gravado o arquivo.
          filetype                = 'BIN' "Definição do tipo do arquivo.
        TABLES
          data_tab                = l_data "Dados Binario do arquivo.
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.

      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        APPEND VALUE #(    mandt  = sy-mandt
                           belnr  = <l_docum>-instid_a
                           gjahr  = p_gjahr-low
                           item   = id
                            log   = sy-msgv1
                            icon  = icon_red_light ) TO t_zmme0002.

      ELSE.
        "Gravar log de executação.
        APPEND VALUE #(    mandt  = sy-mandt
                           belnr  = <l_docum>-instid_a
                           gjahr  = p_gjahr-low
                           item   = id
                            log   = 'PDF gerado c/sucesso-' && name_file
                            icon  = icon_green_light ) TO t_zmme0002.


      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros .

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

  LOOP AT SCREEN.
    git_filtro = VALUE #(
      ( parametro = '' valor = p_belnr )
      ( parametro = '' valor = p_gjahr )
    ).
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100' WITH 'Padrao'.

  PERFORM fm_criar_objetos.
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
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .
  DATA: date TYPE char10.
  date = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
  PERFORM fm_cria_fieldcat.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Download de arquivos vinculado MIRO'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Ano'                valor = p_gjahr-low )
                                                    ( parametro = 'Data processamento' valor = date ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      CHANGING
        it_outtab                     = t_zmme0002
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

*******************************************
  DATA: lc_col_pos  TYPE lvc_colpos.
  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
  CLEAR: git_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZMME0002'
    CHANGING
      ct_fieldcat      = git_fcat.

  LOOP AT git_fcat ASSIGNING <fs_cat>.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
