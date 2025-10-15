*&---------------------------------------------------------------------*
*& Report  ZFICA001
*&
*&---------------------------------------------------------------------*
*&Autor...: Marcelo Ferrari
*&Objetivo: Compensação automática conta transitória / Pedidos de comp.
*&Data....: 29/03/2016
*&---------------------------------------------------------------------*

REPORT  ZFICA001.

*Estrutura para mensagem de erro
TYPES : BEGIN OF ty_s_error,
          msg_err(60) TYPE c,
        END OF ty_s_error.

*Local de arquivos e processamentos
SELECTION-SCREEN BEGIN OF BLOCK blck WITH FRAME TITLE text-011.
PARAMETERS:
  p_file   TYPE rlgrap-filename,                  " Local do arquivo de entrada
  e_file   TYPE rlgrap-filename OBLIGATORY,       " Local para arquivo de erro (log)
  p_mode   TYPE c OBLIGATORY DEFAULT 'N'.         " Modo de processametno
SELECTION-SCREEN END OF BLOCK blck.

* Estrutura de dados
DATA :
  BEGIN OF fs_field,
    agkon TYPE rf05a-agkon,            " Nº conta.
    budat(10) TYPE c,                  " Data lançamento.
    monat TYPE bkpf-monat,             " Período.
    bukrs TYPE bkpf-bukrs,             " Empresa.
    waers TYPE bkpf-waers,             " Moeda.
    sel01 TYPE rf05a-sel01,            " Pedido.
    xblnr TYPE bkpf-xblnr,             " Referencia.
 END OF fs_field.

  DATA:
   t_field    LIKE TABLE OF fs_field,
   t_bdcdata  LIKE TABLE OF bdcdata.

 DATA:
  fs_bdcdata LIKE LINE OF t_bdcdata,   " Structure type of bdcdata
   w_str  TYPE string.

* Declaração de dados
DATA:
  wa_path TYPE string ,
  wa_error TYPE string,
  wa_cnt   TYPE i,
  w_mode    TYPE c,
  wa_cnt1(2) TYPE n,
  it_output type table of ty_s_error,
  wa_output like line of it_output.

AT SELECTION-SCREEN.
* Mode 'A' = Foreground mode
* Mode 'N' = Background mode
  IF p_mode = 'A' OR p_mode = 'N' .

    w_mode = p_mode.

  ELSE.

*mensagem de erro
    MESSAGE 'Entrar o modo de processamento A or N' TYPE 'E'.
  ENDIF.

* Abertura da janela para seleção e local
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_file.

  TYPES:
    fs_struct(4096) TYPE c OCCURS 0 .

  DATA:
    w_struct TYPE fs_struct.

* Uploading excel.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     i_field_seperator          =
*     I_LINE_HEADER              =
      i_tab_raw_data             = w_struct
      i_filename                 = p_file
    TABLES
      i_tab_converted_data       = t_field
    EXCEPTIONS
      conversion_failed          = 1
      OTHERS                     = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Janela / arquivo de erro - download
AT SELECTION-SCREEN ON VALUE-REQUEST FOR e_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = e_file.

* inicio - eventos.
START-OF-SELECTION.

  LOOP AT t_field INTO fs_field .
    REFRESH: t_bdcdata.
    CLEAR fs_bdcdata.
    PERFORM populate_bdcdata.
    PERFORM insert_data.
  ENDLOOP.                               " LOOP AT it_c.

*(popular_bdcdata)***********************

* parte 1
FORM populate_bdcdata.
PERFORM:
fill_bdc_data USING 'SAPMF05A' '0131' 'X'  ' '  ' ',
  fill_bdc_data USING  '' '' ''   'BDC_CURSOR' 'BKPF-MONAT',        " Incicio.
  fill_bdc_data USING  '' '' ''   'BDC_OKCODE' '=SL2',              " Processo.
  fill_bdc_data USING  '' '' ''   'RF05A-AGKON' fs_field-agkon,   " Nº conta.
  fill_bdc_data USING  '' '' ''   'BKPF-BUDAT' fs_field-budat,   " Data lançamento.
  fill_bdc_data USING  '' '' ''   'BKPF-MONAT' fs_field-monat,   " Período.
  fill_bdc_data USING  '' '' ''   'BKPF-BUKRS' fs_field-bukrs,   " Empresa.
  fill_bdc_data USING  '' '' ''   'BKPF-WAERS' fs_field-waers,   " Moeda.

fill_bdc_data USING 'SAPMF05A' '0608' 'X' ' '  ' ' ,
  fill_bdc_data USING  '' '' ''   'BDC_CURSOR'   'RF05A-XPOS1(01)',  " Enter.
  fill_bdc_data USING  '' '' ''   'BDC_OKCODE'   '=P+',              " Enter.

fill_bdc_data USING 'SAPMF05A' '0608' 'X' ' '  ' ' ,
  fill_bdc_data USING  '' '' ''   'BDC_CURSOR'   'RF05A-XPOS1(05)',  " Enter.
  fill_bdc_data USING  '' '' ''   'BDC_OKCODE'   '=ENTR',            " Enter.
  fill_bdc_data USING  '' '' ''   'RF05A-XPOS1(01)'   ' ',           " Enter.
  fill_bdc_data USING  '' '' ''   'RF05A-XPOS1(05)'   'X',           " Enter.

fill_bdc_data USING 'SAPMF05A' '0731' 'X' ' '  ' ' ,
  fill_bdc_data USING  '' '' ''   'BDC_CURSOR'   'RF05A-SEL01(01)',  " Enter.
  fill_bdc_data USING  '' '' ''   'BDC_OKCODE'   '=BS',              " Enter.
  fill_bdc_data USING  '' '' ''   'RF05A-SEL01(01)' fs_field-sel01,  " Enter.

fill_bdc_data USING 'SAPMF05A' '0700' 'X' ' '  ' ' ,
  fill_bdc_data USING  '' '' ''   'BDC_CURSOR'   'BKPF-BKTXT',        " Enter.
  fill_bdc_data USING  '' '' ''   'BKPF-XBLNR' fs_field-xblnr,        " Enter.
  fill_bdc_data USING  '' '' ''   'BDC_OKCODE' '=BU'.                 " Save.

ENDFORM.                               "popular_bdc.

* parte 2
FORM fill_bdc_data USING value(p_program)
                      value(p_dynpro)
                      value(p_dynbegin)
                      value(p_fnam)
                      value(p_fval).
  CLEAR fs_bdcdata .
  IF p_dynbegin = 'X' .
    fs_bdcdata-program = p_program .
    fs_bdcdata-dynpro  = p_dynpro .
    fs_bdcdata-dynbegin = p_dynbegin .
    APPEND fs_bdcdata TO t_bdcdata.
  ELSE.
    fs_bdcdata-fnam = p_fnam.
    fs_bdcdata-fval = p_fval.
    CONDENSE fs_bdcdata-fval.
    APPEND fs_bdcdata TO t_bdcdata.
  ENDIF.                               "

ENDFORM .                              "

*(insert_dados)****************************
FORM insert_data.

*Decl mensagens de erro
  DATA:
       t_msg TYPE TABLE OF bdcmsgcoll,   " Collecting Error messages
       w_msg TYPE bdcmsgcoll,
       w_msg1(51).

* Call transaction 'F-03'
  CALL TRANSACTION 'F-03' USING t_bdcdata
    MODE w_mode
    UPDATE 'S'
    MESSAGES INTO t_msg.

IF sy-subrc EQ 0.

*    Uploaded into the database
    WRITE :/ 'Nº documento', wa_output-msg_err.
  ELSE.

*    Error Found
    LOOP AT t_msg INTO w_msg WHERE msgtyp EQ 'E'.
*     Format Message
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = w_msg-msgid
          msgnr               = w_msg-msgnr
          msgv1               = w_msg-msgv1
          msgv2               = w_msg-msgv2
          msgv3               = w_msg-msgv3
          msgv4               = w_msg-msgv4
        IMPORTING
          message_text_output = w_msg1.

wa_output-msg_err = w_msg1.


*Error message in downloaded file
data:
   wa_string(10) type c.

    wa_string = fs_field-sel01.

    concatenate wa_string wa_output-msg_err into wa_output-msg_err separated by space.

     APPEND wa_output-msg_err TO it_output.

      wa_error = e_file.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
*         BIN_FILESIZE                    =
          filename                        = wa_error
*         FILETYPE                        = 'ASC'
*         APPEND                          = ' '
          write_field_separator           = 'X'
        TABLES
          data_tab                        = it_output
*
                .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.                           " IF sy-subrc <> 0.

    ENDLOOP.

  ENDIF.

ENDFORM.                    "insert_data
