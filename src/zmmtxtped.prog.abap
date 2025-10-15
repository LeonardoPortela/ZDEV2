*&---------------------------------------------------------------------*
*& Report  ZMMTXTPED
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmtxtped.
TABLES ekko.

TYPES:
  BEGIN OF ty_linha,
    ebeln(10)  TYPE c,
    pontov1(1),
    texto(132) TYPE c,
  END OF ty_linha,

  ty_arquivo(143) TYPE c.

DATA it_ekko TYPE TABLE OF ekko.

DATA: it_texto  TYPE STANDARD TABLE OF tline,
      wl_name   TYPE thead-tdname,
      w_linha   TYPE ty_linha,
      w_arquivo TYPE ty_arquivo,
      xchou(1),
      t_arquivo TYPE TABLE OF ty_arquivo.

DATA: wl_filename TYPE rlgrap-filename.

wl_filename = 'C:\PEDIDOS\PEDIDOS.TXT'.

SELECT-OPTIONS: s_ebeln FOR ekko-ebeln.

SELECT *
  FROM ekko
  INTO TABLE it_ekko
  WHERE ebeln IN s_ebeln.

LOOP AT it_ekko INTO DATA(w_ekko).
  wl_name = w_ekko-ebeln.

  REFRESH it_texto.

  CLEAR: w_arquivo,w_linha,xchou.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'F00'
      language                = sy-langu
      name                    = wl_name
      object                  = 'EKKO'
    TABLES
      lines                   = it_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  w_linha-ebeln = w_ekko-ebeln.
  LOOP AT it_texto INTO DATA(w_texto).
    IF sy-tabix > 1 AND xchou IS INITIAL.
      CLEAR w_linha-ebeln.
      xchou = 'X'.
    ENDIF.
    w_linha-texto = w_texto-tdline.
    w_arquivo = w_linha.
    APPEND w_arquivo TO t_arquivo.
  ENDLOOP.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'F01'
      language                = sy-langu
      name                    = wl_name
      object                  = 'EKKO'
    TABLES
      lines                   = it_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.


  LOOP AT it_texto INTO w_texto.
    IF sy-tabix > 1 AND xchou IS INITIAL.
      CLEAR w_linha-ebeln.
      xchou = 'X'.
    ENDIF.
    w_linha-texto = w_texto-tdline.
    w_arquivo = w_linha.
    APPEND w_arquivo TO t_arquivo.
  ENDLOOP.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'F02'
      language                = sy-langu
      name                    = wl_name
      object                  = 'EKKO'
    TABLES
      lines                   = it_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.


  LOOP AT it_texto INTO w_texto.
    IF sy-tabix > 1 AND xchou IS INITIAL.
      CLEAR w_linha-ebeln.
      xchou = 'X'.
    ENDIF.
    w_linha-texto = w_texto-tdline.
    w_arquivo = w_linha.
    APPEND w_arquivo TO t_arquivo.
  ENDLOOP.

ENDLOOP.

CALL FUNCTION 'WS_DOWNLOAD'
  EXPORTING
    filename                = wl_filename
  TABLES
    data_tab                = t_arquivo
  EXCEPTIONS
    file_open_error         = 1
    file_write_error        = 2
    invalid_filesize        = 3
    invalid_type            = 4
    no_batch                = 5
    unknown_error           = 6
    invalid_table_width     = 7
    gui_refuse_filetransfer = 8
    customer_error          = 9
    no_authority            = 10
    OTHERS                  = 11.
