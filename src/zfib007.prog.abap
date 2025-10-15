*&---------------------------------------------------------------------*
*& Report  ZTUTOR003
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zfib007 NO STANDARD PAGE HEADING
                      MESSAGE-ID z01
                      LINE-SIZE 150
                      LINE-COUNT 62(2).
*-----------------------------------------------------------------------
*  Tables, Constants, ranges e others
*-----------------------------------------------------------------------
*
CONSTANTS: c_end_col TYPE i VALUE 60,
           c_mark    TYPE c VALUE 'X',
           c_path    TYPE c LENGTH 200 VALUE '/usr/interfaces/'.

FIELD-SYMBOLS <fs_dataint> TYPE any.

*----------------------------------------------------------------------*
* Declaração para objetos OLE para gravar planilha EXCEL
*----------------------------------------------------------------------*

TYPE-POOLS ole2 .

DATA: h_excel     TYPE ole2_object,        " Excel object
      h_workbooks TYPE ole2_object,        " list of workbooks
      h_work      TYPE ole2_object,        " workbook
      h_cell      TYPE ole2_object.        " cell

DEFINE m_message.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    WHEN OTHERS. RAISE upload_ole.
  ENDCASE.
END-OF-DEFINITION.

FIELD-SYMBOLS: <fs_wa>   TYPE any,
               <fs_comp> TYPE any.

*> RANGES R_ANLKL            FOR ANLA-ANLKL.

*-----------------------------------------------------------------------
*  Variáveis Globais
*-----------------------------------------------------------------------
*
DATA: vg_begin_col    TYPE i VALUE 1,
      vg_begin_row    TYPE i VALUE 1,
      vg_tabix        LIKE sy-tabix,
      v_mess_tab(256) TYPE c,
      vg_ano(4)       TYPE c,
      vg_flag.


DATA: n_dias         TYPE i,
      n_ano          TYPE i,
      n_mes          TYPE i,
      vg_data_fim    TYPE d,
      vg_data_ini    TYPE d,
      vg_data_ini2   TYPE d,
      vg_dias(3)     TYPE c,
      vg_year(3)     TYPE c,
      n_dias_tot     TYPE i,
      v_anbtr_16(13) TYPE c,
      v_util         TYPE i,
      v_depr_mes     TYPE p  DECIMALS 6,
      v_meses_util   TYPE i.

RANGES:  r_arred   FOR anlc-aafap.
*-----------------------------------------------------------------------
*  Tabelas internas e Work areas
*-----------------------------------------------------------------------
*
TYPES: BEGIN OF ty_arquivo,
         bukrs    TYPE c LENGTH 4,     " Empresa
         anlkl    TYPE c LENGTH 8,     " Classe do Imobilizado
         anln1    TYPE c LENGTH 12,    " Nro do Imobilizado
         anln2    TYPE c LENGTH 4,     " Subnº do imobilizado
         txt50    TYPE c LENGTH 50,    " Descrição do Bem
         txa50    TYPE c LENGTH 50,    " Continuação da descrição
         anlhtxt  TYPE c LENGTH 50,    " Texto do Subnº do imob.
         sernr    TYPE c LENGTH 18,    " Número de Série
         invnr    TYPE c LENGTH 25,    " Número de Chassis
         aktiv    TYPE c LENGTH 8,     " Data da incorporação
         gsber    TYPE c LENGTH 4,     " Divisão
         kostl    TYPE c LENGTH 10,    " Centro de Custo
         werks    TYPE c LENGTH 4,     " Centro
         kfzkz    TYPE c LENGTH 15,    " Placa do Veículo
         lifnr    TYPE c LENGTH 10,    " Fornecedor
         ndjar1   TYPE c LENGTH 3,     " Vida Útil em anos
         ndper1   TYPE c LENGTH 3,     " Vida Útil em meses
         ndabj1   TYPE c LENGTH 3,     " Vida Útil exp. em anos
         ndabp1   TYPE c LENGTH 3,     " Vida Útil exp. em meses
         afabg1   TYPE c LENGTH 8,     " Início da Depreciação
         ndjar3   TYPE c LENGTH 3,     " Vida Útil em anos
         ndper3   TYPE c LENGTH 3,     " Vida Útil em meses
         ndabj3   TYPE c LENGTH 3,     " Vida Útil exp. em anos
         ndabp3   TYPE c LENGTH 3,     " Vida Útil exp. em meses
         afabg3   TYPE c LENGTH 8,     " Início da Depreciação
         ndjar4   TYPE c LENGTH 3,     " Vida Útil em anos
         ndper4   TYPE c LENGTH 3,     " Vida Útil em meses
         ndabj4   TYPE c LENGTH 3,     " Vida Útil exp. em anos
         ndabp4   TYPE c LENGTH 3,     " Vida Útil exp. em meses
         afabg4   TYPE c LENGTH 8,     " Início da Depreciação
         ndjar5   TYPE c LENGTH 3,     " Vida Útil em anos
         ndper5   TYPE c LENGTH 3,     " Vida Útil em meses
         ndabj5   TYPE c LENGTH 3,     " Vida Útil exp. em anos
         ndabp5   TYPE c LENGTH 3,     " Vida Útil exp. em meses
         afabg5   TYPE c LENGTH 8,     " Início da Depreciação
         ndjar7   TYPE c LENGTH 3,     " Vida Útil em anos
         ndper7   TYPE c LENGTH 3,     " Vida Útil em meses
         ndabj7   TYPE c LENGTH 3,     " Vida Útil exp. em anos
         ndabp7   TYPE c LENGTH 3,     " Vida Útil exp. em meses
         afabg7   TYPE c LENGTH 8,     " Início da Depreciação
         anbtr1_1 TYPE c LENGTH 13,    " Vlr Aquisição
         anbtr1_6 TYPE c LENGTH 13,    " Depreciação já lançada
         anbtr2_1 TYPE c LENGTH 13,    " Vlr Aquisição PIS
         anbtr2_6 TYPE c LENGTH 13,    " Deprec.já lanç. PIS
         anbtr3_1 TYPE c LENGTH 13,    " Vlr Aquisição INDEX
         anbtr3_6 TYPE c LENGTH 13,    " Deprec.já lanç. INDEX
         anbtr4_1 TYPE c LENGTH 13,    " Vlr Aquisição COFINS
         anbtr4_6 TYPE c LENGTH 13,    " Deprec.já lanç. COFINS
         anbtr5_1 TYPE c LENGTH 13,    " Vlr Aquisição REAV
         anbtr5_6 TYPE c LENGTH 13,    " Deprec.já lanç. REAV
         anbtr6_1 TYPE c LENGTH 13,    " Vlr Aquisição M.FORTE
         anbtr6_6 TYPE c LENGTH 13,    " Deprec.já lanç. M.FORTE
         anbtr7_1 TYPE c LENGTH 13,    " Vlr Aquisição CIAP
         anbtr7_6 TYPE c LENGTH 13,    " Deprec.já lanç. CIAP
       END   OF ty_arquivo.

DATA: BEGIN OF wa_checkfield,
        bukrs    TYPE c,
        anlkl    TYPE c,
        anln1    TYPE c,
        anln2    TYPE c,
        txt50    TYPE c,
        txa50    TYPE c,
        anlhtxt  TYPE c,
        sernr    TYPE c,
        invnr    TYPE c,
        aktiv    TYPE c,
        gsber    TYPE c,
        kostl    TYPE c,
        werks    TYPE c,
        kfzkz    TYPE c,
        lifnr    TYPE c,
        ndjar1   TYPE c,
        ndper1   TYPE c,
        ndabj1   TYPE c,
        ndabp1   TYPE c,
        afabg1   TYPE c,
        ndjar3   TYPE c,
        ndper3   TYPE c,
        ndabj3   TYPE c,
        ndabp3   TYPE c,
        afabg3   TYPE c,
        ndjar4   TYPE c,
        ndper4   TYPE c,
        ndabj4   TYPE c,
        ndabp4   TYPE c,
        afabg4   TYPE c,
        ndjar5   TYPE c,
        ndper5   TYPE c,
        ndabj5   TYPE c,
        ndabp5   TYPE c,
        afabg5   TYPE c,
        ndjar7   TYPE c,
        ndper7   TYPE c,
        ndabj7   TYPE c,
        ndabp7   TYPE c,
        afabg7   TYPE c,
        anbtr1_1 TYPE c,
        anbtr1_8 TYPE c,
        anbtr2_1 TYPE c,
        anbtr3_1 TYPE c,
        anbtr4_1 TYPE c,
        anbtr5_1 TYPE c,
        anbtr6_1 TYPE c,
        anbtr7_1 TYPE c,
        anbtr1_6 TYPE c,
        anbtr2_6 TYPE c,
        anbtr3_6 TYPE c,
        anbtr4_6 TYPE c,
        anbtr5_6 TYPE c,
        anbtr6_6 TYPE c,
        anbtr7_6 TYPE c,
      END   OF wa_checkfield.

DATA: BEGIN OF wa_anla,
        anlkl LIKE anla-anlkl,      "Classe do Imobilizado
        bukrs LIKE anla-bukrs,      "Empresa
        anln1 LIKE anla-anln1,      "Nro do Principal Imob
        anln2 LIKE anla-anln2,      "SubNro do Imob.
      END   OF wa_anla.

DATA: wa_planilha LIKE alsmex_tabline,  "Work Area p/ Planilha
      wa_arquivo  TYPE ty_arquivo,
      wa_arqerror LIKE wa_arquivo.

DATA: it_arquivo  LIKE STANDARD TABLE OF wa_arquivo,
      it_arqerror LIKE STANDARD TABLE OF wa_arquivo,
      it_planilha LIKE STANDARD TABLE OF wa_planilha,
      it_anla     LIKE STANDARD TABLE OF wa_anla,
      r_anlkl     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_anlkl_2   LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_imob1     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_imob2     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_imob3     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_imob4     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_imob5     LIKE RANGE OF anla-anlkl WITH HEADER LINE,
      r_uname     LIKE RANGE OF sy-uname   WITH HEADER LINE.


DATA: it_arq_b    TYPE ty_arquivo       OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
* Declaração de dados para Call Transaction
*----------------------------------------------------------------------*
*
DATA: wa_bdcdata LIKE bdcdata,
      wa_message LIKE bdcmsgcoll,

      it_bdcdata LIKE STANDARD TABLE OF wa_bdcdata,
      it_message LIKE STANDARD TABLE OF wa_message.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s04.
  PARAMETERS: rb_fore RADIOBUTTON GROUP mod DEFAULT 'X',
              rb_back RADIOBUTTON GROUP mod.
SELECTION-SCREEN END   OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_file  LIKE rlgrap-filename OBLIGATORY, "Arq Entrada
              p_errof LIKE rlgrap-filename OBLIGATORY, "Arq Saida
              p_lines LIKE sy-index.
  "Nro Max Linhas
SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_princ RADIOBUTTON GROUP typ DEFAULT 'X',
              p_subnm RADIOBUTTON GROUP typ.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: p_optxt RADIOBUTTON GROUP arq DEFAULT 'X',
              p_opxls RADIOBUTTON GROUP arq.
SELECTION-SCREEN END   OF BLOCK b2.

INCLUDE zbci001.

*----------------------------------------------------------------------*
* At Selection-screen
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Event at selection-screen on value-request p_file
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = p_file
      mask             = '*.*'
      mode             = 'O'
      title            = 'Diretório do arquivo de Entrada'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF ( sy-subrc EQ 3 ).
    MESSAGE i000 WITH 'Seleção cancelada!' 'Pelo usuário' sy-uname.
  ELSEIF ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*----------------------------------------------------------------------*
* Event at selection-screen on value-request p_errof
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_errof.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = p_errof
      mask             = '*.*'
      mode             = 'O'
      title            = 'Diretório do arquivo de Saída'
    IMPORTING
      filename         = p_errof
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF ( sy-subrc EQ 3 ).
    MESSAGE i000 WITH 'Seleção cancelada!' 'Pelo usuário' sy-uname.
  ELSEIF ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*----------------------------------------------------------------------*
* Event at selection-screen on
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON p_lines.

  IF  ( p_lines IS INITIAL ).
    MESSAGE e000 WITH 'Coloque número de linhas!'.
  ENDIF.

*----------------------------------------------------------------------*
* Event at selection-screen on
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN.

*> Inicializa os campos que deverão ser checados os valores iniciais,
*> Caso tenham conteudo devem ser geradas as respectivas entradas nas
*> pastas BI.
  check_modo_bi.

  CLEAR wa_checkfield.
  IF ( p_princ EQ c_mark ).
    wa_checkfield = '               X  XXX  XXX  XXX  XXX  XX'.
  ELSE.
    wa_checkfield = '    XXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'.
  ENDIF.

*----------------------------------------------------------------------*
* Event Initialization.
*----------------------------------------------------------------------*
*
INITIALIZATION.
  SET TITLEBAR 'TITULO'.

  REFRESH r_anlkl.
  CLEAR r_anlkl.
  r_anlkl-sign   = 'I'.
  r_anlkl-option = 'BT'.

  r_anlkl-low    = '10501'.
  r_anlkl-high   = '10505'.
  APPEND r_anlkl.

  r_anlkl-low    = '10701'.
  r_anlkl-high   = '10703'.
  APPEND r_anlkl.

  REFRESH r_anlkl_2.
  CLEAR r_anlkl_2.
  r_anlkl_2-sign   = 'I'.
  r_anlkl_2-option = 'BT'.


  r_anlkl_2-low    = '10501'.
  r_anlkl_2-high   = '10505'.
  APPEND r_anlkl_2.
  r_anlkl_2-low    = '10701'.
  r_anlkl_2-high   = '10703'.
  APPEND r_anlkl_2.

  REFRESH r_imob1.
  CLEAR r_imob1.
  r_imob1-sign   = 'I'.
  r_imob1-option = 'BT'.
  r_imob1-low    = '20101'.
  r_imob1-high   = '20116'.
  APPEND r_imob1.
  r_imob1-low    = '210101'.
  r_imob1-high   = '210102'.
  APPEND r_imob1.
  r_imob1-low    = '210401'.
  r_imob1-high   = '210402'.
  APPEND r_imob1.
  r_imob1-low    = '20000'.
  r_imob1-high   = '20000'.
  APPEND r_imob1.
  r_imob1-low    = '105'.
  r_imob1-high   = '105'.
  APPEND r_imob1.
  r_imob1-low    = '10801'.
  r_imob1-high   = '10801'.
  APPEND r_imob1.

  REFRESH r_imob2.
  CLEAR r_imob2.
  r_imob2-sign   = 'I'.
  r_imob2-option = 'BT'.
  r_imob2-low    = '10201'.
  r_imob2-high   = '10201'.
  APPEND r_imob2.
  r_imob2-low    = '10701'.
  r_imob2-high   = '10703'.
  APPEND r_imob2.
  r_imob2-low    = '11001'.
  r_imob2-high   = '11001'.
  APPEND r_imob2.  .
  r_imob2-low    = '11101'.
  r_imob2-high   = '11101'.
  APPEND r_imob2.
  r_imob2-low    = '11201'.
  r_imob2-high   = '11204'.
  APPEND r_imob2.
  r_imob2-low    = '10301'.
  r_imob2-high   = '10301'.
  APPEND r_imob2.
  r_imob2-low    = '11401'.
  r_imob2-high   = '11401'.
  APPEND r_imob2.
  r_imob2-low    = '40000'.
  r_imob2-high   = '40000'.
  APPEND r_imob2.

  REFRESH r_imob3.
  CLEAR r_imob3.
  r_imob3-sign   = 'I'.
  r_imob3-option = 'BT'.
  r_imob3-low    = '10101'.
  r_imob3-high   = '10103'.
  APPEND r_imob3.
  r_imob3-low    = '10401'.
  r_imob3-high   = '10405'.
  APPEND r_imob3 .
  r_imob3-low    = '10501'.
  r_imob3-high   = '10505'.
  APPEND r_imob3 .
  r_imob3-low    = '10601'.
  r_imob3-high   = '10604'.
  APPEND r_imob3.

  REFRESH r_imob4.
  CLEAR r_imob4.
  r_imob4-sign   = 'I'.
  r_imob4-option = 'BT'.
  r_imob4-low    = '10901'.
  r_imob4-high   = '10904'.
  APPEND r_imob4.

  REFRESH r_imob5.
  CLEAR r_imob5.
  r_imob5-sign   = 'I'.
  r_imob5-option = 'BT'.
  r_imob5-low    = '10301'.
  r_imob5-high   = '10304'.
  APPEND r_imob5.
  r_imob5-low    = '30000'.
  r_imob5-high   = '30000'.
  APPEND r_imob5.

  ini_modo_bi.
*----------------------------------------------------------------------*
* Event Start-of-selection.
*----------------------------------------------------------------------*
*
START-OF-SELECTION.

  PERFORM f_upload_arquivo.
  PERFORM f_seleciona_imobilizado.

  SORT it_anla BY anlkl bukrs anln1 anln2.

  IF NOT rb_fore IS INITIAL.

    LOOP AT it_arquivo INTO wa_arquivo.
      vg_tabix = sy-tabix.
      READ TABLE it_anla INTO wa_anla
                     WITH KEY anlkl = wa_arquivo-anlkl
                              bukrs = wa_arquivo-bukrs
                              anln1 = wa_arquivo-anln1
                              anln2 = wa_arquivo-anln2 BINARY SEARCH.

      IF ( sy-subrc EQ 0 ).
        PERFORM f_imprime_erro USING
                               'Este bem já se encontra cadastrado!'.
        CONTINUE.
      ENDIF.

      PERFORM f_monta_shdb.
    ENDLOOP.

    CHECK ( NOT it_arqerror[] IS INITIAL ).

    PERFORM f_download_arquivo.


  ELSE.

    LOOP AT it_arquivo INTO wa_arquivo.

      it_arq_b = wa_arquivo.
      APPEND it_arq_b.
    ENDLOOP.

    IF NOT it_arq_b[] IS INITIAL.
*Abre sessão do Batch-Input.
      PERFORM bdc_opensessao.

*Processamento do Batch-Input
      PERFORM bdc_processamento.

*Fecha sessão do Bacth-Input.
      PERFORM bdc_closesessao.
    ENDIF.


  ENDIF.


*----------------------------------------------------------------------*
* Event End-of-selection
*----------------------------------------------------------------------*
*
END-OF-SELECTION.
  ULINE.

*----------------------------------------------------------------------*
* Event TOP-OF-PAGE
*----------------------------------------------------------------------*
*
TOP-OF-PAGE.

  ULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/01 'GRUPO ANDRE MAGGI',
         50 TEXT-c01.
  IF ( p_princ EQ c_mark ).
    WRITE 75 'Principal'.
  ELSE.
    WRITE 75 'Subnumero'.
  ENDIF.

  WRITE: 90 TEXT-c02,
         96 sy-datum,
        110 TEXT-c03,
        116 sy-uzeit,
        130 TEXT-c04,
        138 sy-pagno,
        AT sy-linsz ''.

  WRITE:/01 TEXT-c05,
         11 sy-repid,
         50 TEXT-c06,
         58 sy-uname,
         90 TEXT-c07,
         97 sy-mandt,
        AT sy-linsz ''.

  WRITE:/01 TEXT-c08,
         10 p_file,
        AT sy-linsz ''.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  ULINE.
  WRITE:/01(09) TEXT-h01,
         10(04) TEXT-h02,
         16(08) TEXT-h03,
         26(12) TEXT-h04,
         39(4)  TEXT-h05,
         46     TEXT-h06,
        AT sy-linsz ''.
  ULINE.
  SKIP 1.

*----------------------------------------------------------------------*
* Event END-OF-PAGE
*----------------------------------------------------------------------*
*
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  f_upload_arquivo
*&---------------------------------------------------------------------*
*       Rotina de upload de arquivo TXT ou XLS
*----------------------------------------------------------------------*

FORM f_upload_arquivo .

  DATA: vl_filename TYPE string,
        vl_flg_del  TYPE c,
        vl_datum    LIKE sy-datum,
        vl_file     LIKE rlgrap-filename.

  IF ( p_optxt EQ c_mark ).

    IF ( rb_back EQ c_mark ) .



*> Transporta o conteúdo do PATH informado
      vl_file = p_file.

*      OPEN DATASET vl_file IN TEXT MODE ENCODING DEFAULT FOR INPUT
*                                    IGNORING CONVERSION ERRORS
*                                    WITH SMART LINEFEED.

      CALL FUNCTION 'Z_BC_OPENDATASET_WITH_MODE'
        EXPORTING
          filename    = vl_file
          mode        = 'T'
          acess       = 'I'
        TABLES
          table       = it_arquivo
        EXCEPTIONS
          open_error  = 1
          read_error  = 2
          write_error = 3
          close_error = 4
          OTHERS      = 5.

      IF ( sy-subrc NE 0 ).
        CALL FUNCTION 'CUTC_GET_MESSAGE'
          EXPORTING
            msg_id      = sy-msgid
            msg_no      = sy-msgno
            msg_arg1    = sy-msgv1
            msg_arg2    = sy-msgv2
            msg_arg3    = sy-msgv3
            msg_arg4    = sy-msgv4
            language    = sy-langu
          IMPORTING
            raw_message = v_mess_tab.

*        PERFORM f_imprime_erros  USING v_mess_tab.
        CLEAR v_mess_tab.
      ENDIF.

    ELSE.

*> Upload para arquivo no formato TXT

      vl_filename = p_file.

      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = vl_filename
        TABLES
          data_tab                = it_arquivo
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

      IF ( sy-subrc NE 0 ).
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      LOOP AT it_arquivo INTO wa_arquivo.
        WRITE wa_arquivo-bukrs TO wa_arquivo-bukrs RIGHT-JUSTIFIED.
        TRANSLATE wa_arquivo-bukrs USING ' 0'.

      ENDLOOP.

    ENDIF.
  ENDIF.

  IF ( p_opxls EQ c_mark ).

*> Upload para o formato de arquivo XLS.

    REFRESH it_planilha.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = vg_begin_col
        i_begin_row             = vg_begin_row
        i_end_col               = c_end_col
        i_end_row               = p_lines
      TABLES
        intern                  = it_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE e000 WITH TEXT-m01.
    ENDIF.

    SORT it_planilha BY row col.

    LOOP AT it_planilha INTO wa_planilha.

      AT NEW row.
        CLEAR: wa_arquivo, vl_flg_del.
      ENDAT.

      IF ( wa_planilha-value EQ 'DEL' AND wa_planilha-col EQ 1 ).
        vl_flg_del = c_mark.
      ENDIF.

      CHECK ( vl_flg_del NE c_mark ).

      CASE wa_planilha-col.
        WHEN  2.
          WRITE wa_planilha-value TO wa_arquivo-bukrs RIGHT-JUSTIFIED.
          TRANSLATE wa_arquivo-bukrs USING ' 0'.
        WHEN  3. wa_arquivo-anlkl    = wa_planilha-value.
        WHEN  4. wa_arquivo-anln1    = wa_planilha-value.
        WHEN  5. wa_arquivo-anln2    = wa_planilha-value.
        WHEN  6. wa_arquivo-txt50    = wa_planilha-value.
        WHEN  7. wa_arquivo-txa50    = wa_planilha-value.
        WHEN  8. wa_arquivo-anlhtxt  = wa_planilha-value.
        WHEN  9. wa_arquivo-sernr    = wa_planilha-value.
        WHEN  10. wa_arquivo-invnr   = wa_planilha-value.

        WHEN 11 OR 21 OR 26 OR 31 OR 36 OR 41.
*          translate wa_planilha-value using '/.'.
          CASE  wa_planilha-col.
            WHEN 11. ASSIGN ('wa_arquivo-aktiv')  TO <fs_dataint>.
            WHEN 21. ASSIGN ('wa_arquivo-afabg1') TO <fs_dataint>.
            WHEN 26. ASSIGN ('wa_arquivo-afabg3') TO <fs_dataint>.
            WHEN 31. ASSIGN ('wa_arquivo-afabg4') TO <fs_dataint>.
            WHEN 36. ASSIGN ('wa_arquivo-afabg5') TO <fs_dataint>.
            WHEN 41. ASSIGN ('wa_arquivo-afabg7') TO <fs_dataint>.
          ENDCASE.

          IF NOT wa_planilha-value IS INITIAL.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external            = wa_planilha-value
              IMPORTING
                date_internal            = vl_datum
              EXCEPTIONS
                date_external_is_invalid = 1
                OTHERS                   = 2.
            WRITE vl_datum TO <fs_dataint> DDMMYY.

            IF ( sy-subrc NE 0 ).
              MESSAGE e000 WITH 'Erro na conversão de campo data:'.
            ENDIF.
          ENDIF.

        WHEN 12. wa_arquivo-gsber    = wa_planilha-value.
        WHEN 13. wa_arquivo-kostl    = wa_planilha-value.
        WHEN 14. wa_arquivo-werks    = wa_planilha-value.
        WHEN 15. wa_arquivo-kfzkz    = wa_planilha-value.
        WHEN 16. wa_arquivo-lifnr    = wa_planilha-value.
        WHEN 17. wa_arquivo-ndjar1   = wa_planilha-value.
        WHEN 18. wa_arquivo-ndper1   = wa_planilha-value.
        WHEN 19. wa_arquivo-ndabj1   = wa_planilha-value.
        WHEN 20. wa_arquivo-ndabp1   = wa_planilha-value.
        WHEN 21. wa_arquivo-afabg1   = wa_planilha-value.
        WHEN 22. wa_arquivo-ndjar3   = wa_planilha-value.
        WHEN 23. wa_arquivo-ndper3   = wa_planilha-value.
        WHEN 24. wa_arquivo-ndabj3   = wa_planilha-value.
        WHEN 25. wa_arquivo-ndabp3   = wa_planilha-value.
        WHEN 26. wa_arquivo-afabg3   = wa_planilha-value.
        WHEN 27. wa_arquivo-ndjar4   = wa_planilha-value.
        WHEN 28. wa_arquivo-ndper4   = wa_planilha-value.
        WHEN 29. wa_arquivo-ndabj4   = wa_planilha-value.
        WHEN 30. wa_arquivo-ndabp4   = wa_planilha-value.
        WHEN 31. wa_arquivo-afabg4    = wa_planilha-value.
        WHEN 32. wa_arquivo-ndjar5   = wa_planilha-value.
        WHEN 33. wa_arquivo-ndper5   = wa_planilha-value.
        WHEN 34. wa_arquivo-ndabj5   = wa_planilha-value.
        WHEN 35. wa_arquivo-ndabp5   = wa_planilha-value.
        WHEN 36. wa_arquivo-afabg5   = wa_planilha-value.
        WHEN 37. wa_arquivo-ndjar7   = wa_planilha-value.
        WHEN 38. wa_arquivo-ndper7   = wa_planilha-value.
        WHEN 39. wa_arquivo-ndabj7   = wa_planilha-value.
        WHEN 40. wa_arquivo-ndabp7   = wa_planilha-value.
        WHEN 41. wa_arquivo-afabg7   = wa_planilha-value.
        WHEN 42. wa_arquivo-anbtr1_1 = wa_planilha-value.
        WHEN 43. wa_arquivo-anbtr1_6 = wa_planilha-value.
        WHEN 44. wa_arquivo-anbtr2_1 = wa_planilha-value.
        WHEN 45. wa_arquivo-anbtr2_6 = wa_planilha-value.
        WHEN 46. wa_arquivo-anbtr3_1 = wa_planilha-value.
        WHEN 47. wa_arquivo-anbtr3_6 = wa_planilha-value.
        WHEN 48. wa_arquivo-anbtr4_1 = wa_planilha-value.
        WHEN 49. wa_arquivo-anbtr4_6 = wa_planilha-value.
        WHEN 50. wa_arquivo-anbtr5_1 = wa_planilha-value.
        WHEN 51. wa_arquivo-anbtr5_6 = wa_planilha-value.
        WHEN 52. wa_arquivo-anbtr6_1 = wa_planilha-value.
        WHEN 53. wa_arquivo-anbtr6_6 = wa_planilha-value.
        WHEN 54. wa_arquivo-anbtr7_1 = wa_planilha-value.
        WHEN 55. wa_arquivo-anbtr7_6 = wa_planilha-value.
        WHEN OTHERS.
      ENDCASE.

      AT END OF row.
        APPEND wa_arquivo TO it_arquivo.
      ENDAT.
    ENDLOOP.

  ENDIF.




ENDFORM.                    " f_upload_arquivo
*&---------------------------------------------------------------------*
*&      Form  f_seleciona_imobilizado
*&---------------------------------------------------------------------*
*       Seleção de ativo imobilizado para validação de duplicidade
*----------------------------------------------------------------------*

FORM f_seleciona_imobilizado .

  CHECK ( NOT it_arquivo[] IS INITIAL ).

  SELECT anlkl bukrs anln1 anln2
      FROM anla
      INTO TABLE it_anla
       FOR ALL ENTRIES IN it_arquivo
     WHERE ( anlkl EQ it_arquivo-anlkl )
       AND ( bukrs EQ it_arquivo-bukrs )
       AND ( anln1 EQ it_arquivo-anln1 )
       AND ( anln2 EQ it_arquivo-anln2 ).

*---> 04/07/2023 - Migração S4 - WS
  SORT it_anla.
*<--- 04/07/2023 - Migração S4 - WS

  DELETE ADJACENT DUPLICATES FROM it_anla COMPARING ALL FIELDS.

ENDFORM.                    " f_seleciona_imobilizado
*&---------------------------------------------------------------------*
*&      Form  f_monta_shdb
*&---------------------------------------------------------------------*
*       Montagem SHDB
*----------------------------------------------------------------------*

FORM f_monta_shdb .
  DATA vl_message          TYPE c LENGTH 256.

  REFRESH: it_bdcdata, it_message.

*> Para tipo de arquivo (Subnumero)
  IF ( p_subnm EQ c_mark ).
    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '0110',
               ' ' 'BDC_OKCODE'                 ''     '/00',
               ' ' 'ANLA-ANLN1'   wa_checkfield-anlkl  wa_arquivo-anln1,
               ' ' 'ANLA-BUKRS'   wa_checkfield-bukrs  wa_arquivo-bukrs.

  ELSE.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '0105',
               ' ' 'BDC_OKCODE'                 ''     '/00',
               ' ' 'ANLA-ANLKL'   wa_checkfield-anlkl  wa_arquivo-anlkl,
               ' ' 'ANLA-BUKRS'   wa_checkfield-bukrs  wa_arquivo-bukrs.
  ENDIF.


  IF ( p_princ EQ c_mark ) .

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
               ' ' 'BDC_OKCODE'                 ''     '=TAB02',
               ' ' 'ANLA-ANLN1'   wa_checkfield-anln1  wa_arquivo-anln1,
               ' ' 'ANLA-TXT50'   wa_checkfield-txt50  wa_arquivo-txt50,
               ' ' 'ANLA-TXA50'   wa_checkfield-txa50  wa_arquivo-txa50,
               ' ' 'ANLA-SERNR'   wa_checkfield-sernr  wa_arquivo-sernr.
    IF wa_arquivo-aktiv+4(4) = '2007' OR
       wa_arquivo-aktiv+4(4) = '07'.
      PERFORM f_bdc_insert USING:
            ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  '31122006'.
    ELSE.
      PERFORM f_bdc_insert USING:
            ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  wa_arquivo-aktiv.

    ENDIF.
  ELSE.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
             ' ' 'BDC_OKCODE'                 ''     '=TAB02',

             ' ' 'ANLA-TXT50'   wa_checkfield-txt50  wa_arquivo-txt50,
             ' ' 'ANLA-TXA50'   wa_checkfield-txa50  wa_arquivo-txa50,
             ' ' 'ANLA-SERNR'   wa_checkfield-sernr  wa_arquivo-sernr.
    IF wa_arquivo-aktiv+4(4) = '2007' OR
       wa_arquivo-aktiv+4(4) = '07'.
      PERFORM f_bdc_insert USING:
            ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  '31122006'.
    ELSE.
      PERFORM f_bdc_insert USING:
            ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  wa_arquivo-aktiv.
    ENDIF.


  ENDIF.
*> Para tipo de arquivo (Subnumero)
  IF ( p_subnm EQ c_mark ).
    PERFORM f_bdc_insert USING
*            ' ' 'ANLH-ANLHTXT' wa_checkfield-anlhtxt wa_arquivo-anlhtxt
            ' ' 'ANLA-TXT50' wa_checkfield-anlhtxt wa_arquivo-anlhtxt.
*.
*.
  ENDIF.

*> Para classes de imobilizado específicas
  IF ( wa_arquivo-anlkl IN r_anlkl_2 ).
    PERFORM f_bdc_insert USING:
            ' ' 'ANLA-INVNR'   wa_checkfield-invnr   wa_arquivo-invnr.
  ENDIF.

  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
               ' ' 'BDC_OKCODE'               ''     '=TAB03',
               ' ' 'ANLZ-GSBER' wa_checkfield-gsber  wa_arquivo-gsber,
               ' ' 'ANLZ-KOSTL' wa_checkfield-kostl  wa_arquivo-kostl,
               ' ' 'ANLZ-WERKS' wa_checkfield-werks  wa_arquivo-werks.

*> Para classes de imobilizado específicas
  IF ( wa_arquivo-anlkl IN r_anlkl ).
    PERFORM f_bdc_insert USING
               ' ' 'ANLZ-KFZKZ' wa_checkfield-kfzkz  wa_arquivo-kfzkz.
  ENDIF.

  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'BDC_OKCODE'               ''      '=TAB04'.


  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'BDC_OKCODE'               ''      '=TAB06',
               ' ' 'ANLA-LIFNR'               ''  wa_arquivo-lifnr.

  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'BDC_OKCODE'               ''      '=TAB07'.

  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'BDC_OKCODE'               ''      '=TAB08'.

  PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
         ' ' 'BDC_OKCODE'               ''      '=ALTD'.

  IF NOT wa_arquivo-ndjar1 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDJAR(01)' wa_checkfield-ndjar1 wa_arquivo-ndjar1.
  ENDIF.
  IF NOT wa_arquivo-ndjar3 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDJAR(03)' wa_checkfield-ndjar3 wa_arquivo-ndjar3.
  ENDIF.

  IF NOT wa_arquivo-ndjar4 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDJAR(04)' wa_checkfield-ndjar4 wa_arquivo-ndjar4.
  ENDIF.

  IF NOT wa_arquivo-ndjar5 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDJAR(05)' wa_checkfield-ndjar5 wa_arquivo-ndjar5.
  ENDIF.

  IF NOT wa_arquivo-ndjar7 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDJAR(07)' wa_checkfield-ndjar7 wa_arquivo-ndjar7.
  ENDIF.

  IF NOT wa_arquivo-ndper1 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDPER(01)' wa_checkfield-ndper1 wa_arquivo-ndper1.
  ENDIF.

  IF NOT wa_arquivo-ndper3 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDPER(03)' wa_checkfield-ndper3 wa_arquivo-ndper3.
  ENDIF.

  IF NOT wa_arquivo-ndper4 IS INITIAL.
    PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDPER(04)' wa_checkfield-ndper4 wa_arquivo-ndper4.
  ENDIF.


  IF NOT wa_arquivo-ndper5 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-NDPER(05)' wa_checkfield-ndper5 wa_arquivo-ndper5.
  ENDIF.

  IF NOT wa_arquivo-ndper7 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-NDPER(07)' wa_checkfield-ndper7 wa_arquivo-ndper7.
  ENDIF.

  IF NOT wa_arquivo-afabg1 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(01)' wa_checkfield-afabg1 wa_arquivo-afabg1.
  ELSE.
    PERFORM f_bdc_insert USING:
' ' 'ANLB-AFABG(01)' wa_checkfield-afabg1 wa_arquivo-aktiv.

  ENDIF.

  IF NOT wa_arquivo-afabg3 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(03)' wa_checkfield-afabg1 wa_arquivo-afabg3.
  ELSE.
    PERFORM f_bdc_insert USING:
' ' 'ANLB-AFABG(03)' wa_checkfield-afabg1 wa_arquivo-aktiv.
  ENDIF.

  IF NOT wa_arquivo-afabg4 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(04)' wa_checkfield-afabg1 wa_arquivo-afabg4.
  ELSE.
    PERFORM f_bdc_insert USING:
' ' 'ANLB-AFABG(04)' wa_checkfield-afabg1 wa_arquivo-aktiv.
  ENDIF.


  IF NOT wa_arquivo-afabg5 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(05)' wa_checkfield-afabg1 wa_arquivo-afabg5.
  ELSE.
    PERFORM f_bdc_insert USING:
' ' 'ANLB-AFABG(05)' wa_checkfield-afabg1 wa_arquivo-aktiv.
  ENDIF.

  IF NOT wa_arquivo-afabg7 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(07)' wa_checkfield-afabg1 wa_arquivo-afabg7.
  ELSE.
    PERFORM f_bdc_insert USING:
' ' 'ANLB-AFABG(07)' wa_checkfield-afabg1 wa_arquivo-aktiv.
  ENDIF.


  IF NOT wa_arquivo-ndabj1 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABJ(01)' wa_checkfield-ndabj1 wa_arquivo-ndabj1.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_ano GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(01)'  wa_checkfield-ndabj1       vg_year.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabj3 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABJ(03)' wa_checkfield-ndabj1 wa_arquivo-ndabj3.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_ano GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(03)'  ''       vg_year.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabj4 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABJ(04)' wa_checkfield-ndabj1 wa_arquivo-ndabj4.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_ano GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(04)'  ''       vg_year.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabj5 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABJ(05)' wa_checkfield-ndabj1 wa_arquivo-ndabj5.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_ano GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(05)'  ''       vg_year.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabj7 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABJ(07)' wa_checkfield-ndabj1 wa_arquivo-ndabj7.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_ano GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(07)'  ''       vg_year.
    ENDIF.
  ENDIF.


  IF NOT wa_arquivo-ndabp1 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABP(01)' wa_checkfield-ndabp1 wa_arquivo-ndabp1.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_dias GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(01)'  wa_checkfield-ndabp1       vg_dias.
    ENDIF.

  ENDIF.

  IF NOT wa_arquivo-ndabp3 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABP(03)' wa_checkfield-ndabp1 wa_arquivo-ndabp3.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_dias GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(03)'  ''       vg_dias.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabp4 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABP(04)' wa_checkfield-ndabp1 wa_arquivo-ndabp4.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_dias GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(04)'  ''       vg_dias.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabp5 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABP(05)' wa_checkfield-ndabp1 wa_arquivo-ndabp5.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_dias GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(05)'  ''       vg_dias.
    ENDIF.
  ENDIF.

  IF NOT wa_arquivo-ndabp7 IS INITIAL.
    PERFORM f_bdc_insert USING:
  ' ' 'ANLC-NDABP(07)' wa_checkfield-ndabp1 wa_arquivo-ndabp7.
  ELSE.
    PERFORM f_calcula_data.
    IF vg_dias GT 0.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(07)'  ''       vg_dias.
    ENDIF.
  ENDIF.


  vg_ano =  wa_arquivo-aktiv+4(4).

  IF vg_ano = '2007' OR
     vg_ano = '07'.


    PERFORM f_bdc_insert USING: 'X' 'SAPLALTD'  ''    '1100',
        ' ' 'BDC_OKCODE'                     ''    '=MAST',
 ' ' 'RALT_DYNP_STRUC-ANBTR01(01)'      wa_checkfield-anbtr1_1
                                           wa_arquivo-anbtr1_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR01(16)'      wa_checkfield-anbtr1_6
                                           wa_arquivo-anbtr1_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR02(01)'      wa_checkfield-anbtr2_1
                                           wa_arquivo-anbtr2_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR02(16)'      wa_checkfield-anbtr2_6
                                           wa_arquivo-anbtr2_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR03(01)'      wa_checkfield-anbtr3_1
                                           wa_arquivo-anbtr3_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR03(16)'      wa_checkfield-anbtr3_6
                                           wa_arquivo-anbtr3_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR04(01)'      wa_checkfield-anbtr4_1
                                           wa_arquivo-anbtr4_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR04(16)'      wa_checkfield-anbtr4_6
                                           wa_arquivo-anbtr4_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR05(01)'      wa_checkfield-anbtr5_1
                                           wa_arquivo-anbtr5_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR05(16)'      wa_checkfield-anbtr5_6
                                           wa_arquivo-anbtr5_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR06(01)'      wa_checkfield-anbtr6_1
                                           wa_arquivo-anbtr6_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR06(16)'      wa_checkfield-anbtr6_6
                                           wa_arquivo-anbtr6_6,
 ' ' 'RALT_DYNP_STRUC-ANBTR07(01)'      wa_checkfield-anbtr7_1
                                           wa_arquivo-anbtr7_1,
 ' ' 'RALT_DYNP_STRUC-ANBTR07(16)'      wa_checkfield-anbtr7_6
                                           wa_arquivo-anbtr7_6.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                 ' ' 'BDC_OKCODE'               ''      '=TAB01'.
    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  wa_arquivo-aktiv,
                 ' ' 'BDC_OKCODE'                     ''    '=BUCH'.


    CLEAR vg_ano.

  ELSE.


    PERFORM f_bdc_insert USING: 'X' 'SAPLALTD'  ''    '1100',
           ' ' 'BDC_OKCODE'                     ''    '=BUCH'.


    PERFORM f_calcula_valor USING      wa_arquivo-anbtr1_1
                                       wa_arquivo-anbtr1_6
                                       wa_arquivo-ndjar1
                                       wa_arquivo-ndper1
                                       ' '

                             CHANGING  wa_arquivo-anbtr1_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR01(01)'      wa_checkfield-anbtr1_1
                                              wa_arquivo-anbtr1_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR01(06)'      wa_checkfield-anbtr1_6
                                              wa_arquivo-anbtr1_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR01(16)'      wa_checkfield-anbtr1_6
                                              v_anbtr_16.

    PERFORM f_calcula_valor USING      wa_arquivo-anbtr2_1
                                       wa_arquivo-anbtr2_6
                                       wa_arquivo-ndjar4
                                       wa_arquivo-ndper4
                                       ''
                             CHANGING  wa_arquivo-anbtr2_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR02(01)'      wa_checkfield-anbtr2_1
                                              wa_arquivo-anbtr2_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR02(06)'      wa_checkfield-anbtr2_6
                                              wa_arquivo-anbtr2_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR02(16)'      wa_checkfield-anbtr2_6
                                              v_anbtr_16.


    PERFORM f_calcula_valor USING     wa_arquivo-anbtr3_1
                                       wa_arquivo-anbtr3_6
                                       wa_arquivo-ndjar1
                                       wa_arquivo-ndper1
                                       ''
                             CHANGING  wa_arquivo-anbtr3_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR03(01)'      wa_checkfield-anbtr3_1
                                              wa_arquivo-anbtr3_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR03(06)'      wa_checkfield-anbtr3_6
                                              wa_arquivo-anbtr3_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR03(16)'      wa_checkfield-anbtr3_6
                                              v_anbtr_16.

    PERFORM f_calcula_valor USING      wa_arquivo-anbtr4_1
                                       wa_arquivo-anbtr4_6
                                       wa_arquivo-ndjar5
                                       wa_arquivo-ndper5
                                       ''
                             CHANGING  wa_arquivo-anbtr4_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR04(01)'      wa_checkfield-anbtr4_1
                                              wa_arquivo-anbtr4_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR04(06)'      wa_checkfield-anbtr4_6
                                              wa_arquivo-anbtr4_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR04(16)'      wa_checkfield-anbtr4_6
                                              v_anbtr_16.

    PERFORM f_calcula_valor USING      wa_arquivo-anbtr5_1
                                       wa_arquivo-anbtr5_6
                                       wa_arquivo-ndjar3
                                       wa_arquivo-ndper3
                                       wa_arquivo-afabg3
                             CHANGING  wa_arquivo-anbtr5_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR05(01)'      wa_checkfield-anbtr5_1
                                              wa_arquivo-anbtr5_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR05(06)'      wa_checkfield-anbtr5_6
                                              wa_arquivo-anbtr5_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR05(16)'      wa_checkfield-anbtr5_6
                                              v_anbtr_16.

    PERFORM f_calcula_valor USING     wa_arquivo-anbtr6_1
                                       wa_arquivo-anbtr6_6
                                       wa_arquivo-ndjar1
                                       wa_arquivo-ndper1
                                       ''
                             CHANGING  wa_arquivo-anbtr6_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR06(01)'      wa_checkfield-anbtr6_1
                                              wa_arquivo-anbtr6_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR06(06)'      wa_checkfield-anbtr6_6
                                              wa_arquivo-anbtr6_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR06(16)'      wa_checkfield-anbtr6_6
                                              v_anbtr_16.

    vg_flag = 'X'.
    PERFORM f_calcula_valor USING      wa_arquivo-anbtr7_1
                                       wa_arquivo-anbtr7_6
                                       wa_arquivo-ndjar7
                                       wa_arquivo-ndper7
                                       ''
                             CHANGING  wa_arquivo-anbtr7_6.
    PERFORM f_bdc_insert USING:
    ' ' 'RALT_DYNP_STRUC-ANBTR07(01)'      wa_checkfield-anbtr7_1
                                              wa_arquivo-anbtr7_1,
    ' ' 'RALT_DYNP_STRUC-ANBTR07(06)'      wa_checkfield-anbtr7_6
                                              wa_arquivo-anbtr7_6,
    ' ' 'RALT_DYNP_STRUC-ANBTR07(16)'      wa_checkfield-anbtr7_6
                                              v_anbtr_16.
    CLEAR: vg_flag.

  ENDIF.


  IF ( p_princ EQ c_mark ).
    CALL TRANSACTION 'AS91' USING it_bdcdata
                             MODE p_mod1
                           UPDATE 'S'
                         MESSAGES INTO it_message.

  ELSE.
    CALL TRANSACTION 'AS94' USING it_bdcdata
                             MODE p_mod1
                           UPDATE 'S'
                         MESSAGES INTO it_message.

  ENDIF.

*> Verifica se houve erro no processamento das pastas Batch-Input
*> e grava os registros na tabela de retorno de erros.
  READ TABLE it_message INTO wa_message
                   WITH KEY msgtyp = 'E'.
  IF ( sy-subrc EQ 0 ).
    wa_arqerror = wa_arquivo.
    APPEND wa_arqerror TO it_arqerror.
  ENDIF.


  LOOP AT it_message INTO wa_message.
    IF wa_message-msgtyp = 'E' OR
       wa_message-msgtyp = 'S'.

      sy-msgno = wa_message-msgnr.
      sy-msgv1 = wa_message-msgv1.
      sy-msgv2 = wa_message-msgv2.
      sy-msgv3 = wa_message-msgv3.
      sy-msgv4 = wa_message-msgv4.

      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          msg_type       = wa_message-msgtyp
          msg_id         = wa_message-msgid
          msg_no         = sy-msgno
          msg_arg1       = sy-msgv1
          msg_arg2       = sy-msgv2
          msg_arg3       = sy-msgv3
          msg_arg4       = sy-msgv4
        IMPORTING
          raw_message    = vl_message
        EXCEPTIONS
          msg_not_found  = 1
          internal_error = 2
          OTHERS         = 3.

      IF ( sy-subrc NE 0 ).
        vl_message = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      PERFORM f_imprime_erro USING vl_message.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " f_monta_shdb
*&---------------------------------------------------------------------*
*&      Form  f_bdc_insert
*&---------------------------------------------------------------------*
*       Inserir dados das pastas Batch-Input
*----------------------------------------------------------------------*
*  -->  p1 Código da pasta BI
*  -->  p2 Nome do campo  / Nome do programa qdo Código igual a 'X'
*  -->  p3 Valor do campo / Tela do programa qdo Código igual a 'X'
*----------------------------------------------------------------------*
FORM f_bdc_insert USING VALUE(p_codigo)
                        VALUE(p_fnam)
                        VALUE(p_test)
                        VALUE(p_fval).

  CLEAR wa_bdcdata.
  wa_bdcdata-dynbegin = p_codigo.
  IF ( p_codigo EQ 'X' ).
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
  ELSE.
    wa_bdcdata-fnam     = p_fnam.
    wa_bdcdata-fval     = p_fval.
  ENDIF.
  IF ( NOT p_test IS INITIAL ).
    IF ( NOT p_fval IS INITIAL ).
      APPEND wa_bdcdata TO it_bdcdata.
    ENDIF.
  ELSE.
    APPEND wa_bdcdata TO it_bdcdata.
  ENDIF.
ENDFORM.                    " f_bdc_insert
*&---------------------------------------------------------------------*
*&      Form  f_imprime_erro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_MESSAGE  text
*----------------------------------------------------------------------*
FORM f_imprime_erro  USING  VALUE(p_message).
  STATICS vl_color         TYPE n.

  WRITE:/01(06) vg_tabix         COLOR COL_KEY,
         10     wa_arquivo-bukrs COLOR COL_KEY,
         16     wa_arquivo-anlkl COLOR COL_KEY,
         26     wa_arquivo-anln1 COLOR COL_KEY,
         39     wa_arquivo-anln2 COLOR COL_KEY.

  FORMAT COLOR COL_KEY INTENSIFIED ON.


  SET LEFT SCROLL-BOUNDARY.

  IF ( vl_color EQ 1 ).
    WRITE  46     p_message        COLOR COL_NORMAL INTENSIFIED ON.
    CLEAR vl_color.
  ELSE.
    WRITE  46     p_message        COLOR COL_NORMAL INTENSIFIED OFF.
    vl_color = 1.
  ENDIF.

ENDFORM.                    " f_imprime_erro
*&---------------------------------------------------------------------*
*&      Form  f_download_arquivo
*&---------------------------------------------------------------------*
FORM f_download_arquivo .

  DATA: vl_filename TYPE string,
        vl_flg_del  TYPE c,
        vl_datum    LIKE sy-datum,
        vl_l        TYPE i,
        vl_c        TYPE i.

  IF ( p_optxt EQ c_mark ).
*> Upload para arquivo no formato TXT

    vl_filename = p_errof.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename = vl_filename
      TABLES
        data_tab = it_arqerror
      EXCEPTIONS
        OTHERS   = 4.

    IF ( sy-subrc NE 0 ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
*> Download para o formato de arquivo XLS.

*
* Carrega planilha com dados de clientes
*

* start Excel
    IF h_excel-header = space OR h_excel-handle = -1.
      CREATE OBJECT h_excel 'EXCEL.APPLICATION'.
      m_message.
    ENDIF.



*    set property of h_excel  'Visible' = 1.
*    m_message.
*
* get list of workbooks, initially empty
    CALL METHOD OF h_excel 'Workbooks' = h_workbooks.
    m_message.

* add a new workbook
    CALL METHOD OF h_workbooks 'Add' = h_work.
    m_message.


    LOOP AT it_arqerror INTO wa_arqerror.
      vl_l = sy-tabix.
      ASSIGN wa_arqerror TO <fs_wa>.

      CLEAR vl_c.
      DO 54 TIMES.
        vl_c = vl_c + 1.
        CHECK ( vl_c GT 1 ).
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_wa> TO <fs_comp>.

        CALL METHOD OF h_excel 'Cells' = h_cell
          EXPORTING
          #1 = vl_l
          #2 = vl_c.
        m_message.
        SET PROPERTY OF h_cell  'Value' = <fs_comp>.
        m_message.

      ENDDO.
    ENDLOOP.



    SET PROPERTY OF h_excel 'DisplayAlerts' = 0.
    m_message.
    CALL METHOD OF h_work 'SaveAs'
      EXPORTING
        #1 = p_errof.
    m_message.
    CALL METHOD OF h_excel 'QUIT'.
    FREE OBJECT h_excel.
    FREE OBJECT h_work.
    FREE OBJECT h_workbooks.
    FREE OBJECT h_cell.
    m_message.

  ENDIF.
ENDFORM.                    " f_download_arquivo

*&---------------------------------------------------------------------*
*&      Form  bdc_opensessao
*&---------------------------------------------------------------------*
*       Abre o Batch-Input
*----------------------------------------------------------------------*
FORM bdc_opensessao .


  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = 'CARGA_ATIVO'
      holddate            = '00000000' "sy-datum
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.


ENDFORM.                    " bdc_opensessao


*&---------------------------------------------------------------------*
*&      Form  bdc_closesessao
*&---------------------------------------------------------------------*
*       Fecha sessao do Batch-Input
*----------------------------------------------------------------------*

FORM bdc_closesessao .

  CALL FUNCTION 'BDC_CLOSE_GROUP'.

ENDFORM.                    " bdc_closesessao


*&---------------------------------------------------------------------*
*&      Form  bdc_transacao
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_transacao : Código da transação
*----------------------------------------------------------------------*
FORM bdc_transacao  USING p_transacao.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode            = p_transacao
    TABLES
      dynprotab        = it_bdcdata
    EXCEPTIONS
      internal_error   = 1
      not_open         = 2
      queue_error      = 3
      tcode_invalid    = 4
      printing_invalid = 5
      posting_invalid  = 6
      OTHERS           = 7.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.                    " bdc_transacao

*&---------------------------------------------------------------------*
*&      Form  bdc_processamento
*&---------------------------------------------------------------------*
*       Processamento Batch-Input
*----------------------------------------------------------------------*

FORM bdc_processamento .

  LOOP AT it_arq_b.

    CLEAR: it_bdcdata.
    FREE : it_bdcdata.


*> Para tipo de arquivo (Subnumero)
    IF ( p_subnm EQ c_mark ).
      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '0110',
                 ' ' 'BDC_OKCODE'                 ''     '/00',
                 ' ' 'ANLA-ANLN1'   wa_checkfield-anlkl  it_arq_b-anln1,
                 ' ' 'ANLA-BUKRS'   wa_checkfield-bukrs  it_arq_b-bukrs.

    ELSE.

      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '0105',
                 ' ' 'BDC_OKCODE'                 ''     '/00',
                 ' ' 'ANLA-ANLKL'   wa_checkfield-anlkl  it_arq_b-anlkl,
                 ' ' 'ANLA-BUKRS'   wa_checkfield-bukrs  it_arq_b-bukrs.
    ENDIF.


    IF ( p_princ EQ c_mark ) .
      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
                 ' ' 'BDC_OKCODE'                 ''     '=TAB02',
                 ' ' 'ANLA-ANLN1'   wa_checkfield-anln1  it_arq_b-anln1,
                 ' ' 'ANLA-TXT50'   wa_checkfield-txt50  it_arq_b-txt50,
                 ' ' 'ANLA-TXA50'   wa_checkfield-txa50  it_arq_b-txa50,
                 ' ' 'ANLA-SERNR'   wa_checkfield-sernr  it_arq_b-sernr.

      IF it_arq_b-aktiv+4(4) = '2007' OR
         it_arq_b-aktiv+4(2) = '07'.

        PERFORM f_bdc_insert USING:
              ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  '31122006'.
      ELSE.
        PERFORM f_bdc_insert USING:
              ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  it_arq_b-aktiv.

      ENDIF.

*                ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  it_arq_b-aktiv.
    ELSE.

      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
               ' ' 'BDC_OKCODE'                 ''     '=TAB02',

               ' ' 'ANLA-TXT50'   wa_checkfield-txt50  it_arq_b-txt50,
               ' ' 'ANLA-TXA50'   wa_checkfield-txa50  it_arq_b-txa50,
               ' ' 'ANLA-SERNR'   wa_checkfield-sernr  it_arq_b-sernr.
*               ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  it_arq_b-aktiv.

      IF it_arq_b-aktiv+4(4) = '2007' OR
         it_arq_b-aktiv+4(2) = '07'.
        PERFORM f_bdc_insert USING:
              ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  '31122006'.

      ELSE.
        PERFORM f_bdc_insert USING:
              ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  it_arq_b-aktiv.

      ENDIF.
    ENDIF.
*> Para tipo de arquivo (Subnumero)
    IF ( p_subnm EQ c_mark ).
      PERFORM f_bdc_insert USING
              ' ' 'ANLH-ANLHTXT' wa_checkfield-anlhtxt it_arq_b-anlhtxt.
    ENDIF.

*> Para classes de imobilizado específicas
    IF ( it_arq_b-anlkl IN r_anlkl_2 ).
      PERFORM f_bdc_insert USING:
               ' ' 'ANLA-INVNR'   wa_checkfield-invnr   it_arq_b-invnr.
    ENDIF.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''     '1000',
                 ' ' 'BDC_OKCODE'               ''     '=TAB03',
                 ' ' 'ANLZ-GSBER' wa_checkfield-gsber  it_arq_b-gsber,
                 ' ' 'ANLZ-KOSTL' wa_checkfield-kostl  it_arq_b-kostl,
                 ' ' 'ANLZ-WERKS' wa_checkfield-werks  it_arq_b-werks.

*> Para classes de imobilizado específicas
    IF ( it_arq_b-anlkl IN r_anlkl ).
      PERFORM f_bdc_insert USING
                 ' ' 'ANLZ-KFZKZ' wa_checkfield-kfzkz  it_arq_b-kfzkz.
    ENDIF.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                 ' ' 'BDC_OKCODE'               ''      '=TAB04'.


    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                 ' ' 'BDC_OKCODE'               ''      '=TAB06',
                 ' ' 'ANLA-LIFNR'               ''  it_arq_b-lifnr.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                 ' ' 'BDC_OKCODE'               ''      '=TAB07'.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                 ' ' 'BDC_OKCODE'               ''      '=TAB08'.

    PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
           ' ' 'BDC_OKCODE'               ''      '=ALTD'.

    IF NOT it_arq_b-ndjar1 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDJAR(01)' wa_checkfield-ndjar1 it_arq_b-ndjar1.
    ENDIF.
    IF NOT it_arq_b-ndjar3 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDJAR(03)' wa_checkfield-ndjar3 it_arq_b-ndjar3.
    ENDIF.

    IF NOT it_arq_b-ndjar4 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDJAR(04)' wa_checkfield-ndjar4 it_arq_b-ndjar4.
    ENDIF.

    IF NOT it_arq_b-ndjar5 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDJAR(05)' wa_checkfield-ndjar5 it_arq_b-ndjar5.
    ENDIF.

    IF NOT it_arq_b-ndjar7 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDJAR(07)' wa_checkfield-ndjar7 it_arq_b-ndjar7.
    ENDIF.

    IF NOT it_arq_b-ndper1 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDPER(01)' wa_checkfield-ndper1 it_arq_b-ndper1.
    ENDIF.

    IF NOT it_arq_b-ndper3 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDPER(03)' wa_checkfield-ndper3 it_arq_b-ndper3.
    ENDIF.

    IF NOT it_arq_b-ndper4 IS INITIAL.
      PERFORM f_bdc_insert USING:
      ' ' 'ANLB-NDPER(04)' wa_checkfield-ndper4 it_arq_b-ndper4.
    ENDIF.


    IF NOT it_arq_b-ndper5 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDPER(05)' wa_checkfield-ndper5 it_arq_b-ndper5.
    ENDIF.

    IF NOT it_arq_b-ndper7 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-NDPER(07)' wa_checkfield-ndper7 it_arq_b-ndper7.
    ENDIF.

    IF NOT it_arq_b-afabg1 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-AFABG(01)' wa_checkfield-afabg1 it_arq_b-afabg1.
    ELSE.
      PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(01)' wa_checkfield-ndper5  it_arq_b-aktiv.
    ENDIF.

    IF NOT it_arq_b-afabg3 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-AFABG(03)' wa_checkfield-afabg1 it_arq_b-afabg3.
    ELSE.
      PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(03)' wa_checkfield-ndper5  it_arq_b-aktiv.
    ENDIF.

    IF NOT it_arq_b-afabg4 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-AFABG(04)' wa_checkfield-afabg1 it_arq_b-afabg4.
    ELSE.
      PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(04)' wa_checkfield-ndper5  it_arq_b-aktiv.
    ENDIF.


    IF NOT it_arq_b-afabg5 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-AFABG(05)' wa_checkfield-afabg1 it_arq_b-afabg5.
    ELSE.
      PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(05)' wa_checkfield-ndper5  it_arq_b-aktiv.
    ENDIF.

    IF NOT it_arq_b-afabg7 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLB-AFABG(07)' wa_checkfield-afabg1 it_arq_b-afabg7.
    ELSE.
      PERFORM f_bdc_insert USING:
  ' ' 'ANLB-AFABG(07)' wa_checkfield-ndper5  it_arq_b-aktiv.
    ENDIF.


    IF NOT it_arq_b-ndabj1 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(01)' wa_checkfield-ndabj1 it_arq_b-ndabj1.
    ELSE.
      PERFORM f_calcula_data.
      IF n_ano GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABJ(01)'  wa_checkfield-ndabj1       vg_year.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabj3 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(03)' wa_checkfield-ndabj1 it_arq_b-ndabj3.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_ano GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABJ(03)'  wa_checkfield-ndabj1       vg_year.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabj4 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(04)' wa_checkfield-ndabj1 it_arq_b-ndabj4.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_ano GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABJ(04)'  wa_checkfield-ndabj1       vg_year.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabj5 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(05)' wa_checkfield-ndabj1 it_arq_b-ndabj5.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_ano GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABJ(05)'  wa_checkfield-ndabj1       vg_year.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabj7 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABJ(07)' wa_checkfield-ndabj1 it_arq_b-ndabj7.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_ano GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABJ(07)'  wa_checkfield-ndabj1       vg_year.
      ENDIF.
    ENDIF.


    IF NOT it_arq_b-ndabp1 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(01)' wa_checkfield-ndabp1 it_arq_b-ndabp1.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_dias GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABP(01)'  wa_checkfield-ndabp1       vg_dias.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabp3 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(03)' wa_checkfield-ndabp1 it_arq_b-ndabp3.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_dias GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABP(03)'  wa_checkfield-ndabp1       vg_dias.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabp4 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(04)' wa_checkfield-ndabp1 it_arq_b-ndabp4.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_dias GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABP(04)'  wa_checkfield-ndabp1       vg_dias.
      ENDIF.

    ENDIF.

    IF NOT it_arq_b-ndabp5 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(05)' wa_checkfield-ndabp1 it_arq_b-ndabp5.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_dias GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABP(05)'  wa_checkfield-ndabp1       vg_dias.
      ENDIF.
    ENDIF.

    IF NOT it_arq_b-ndabp7 IS INITIAL.
      PERFORM f_bdc_insert USING:
    ' ' 'ANLC-NDABP(07)' wa_checkfield-ndabp1 it_arq_b-ndabp7.
    ELSE.
      PERFORM f_calcula_data.
      IF vg_dias GT 0.
        PERFORM f_bdc_insert USING:
      ' ' 'ANLC-NDABP(07)'  wa_checkfield-ndabp1       vg_dias.
      ENDIF.
    ENDIF.



    vg_ano =  wa_arquivo-aktiv+4(4).

    IF vg_ano = '2007' OR
       vg_ano = '07'.

      PERFORM f_bdc_insert USING: 'X' 'SAPLALTD'  ''    '1100',
          ' ' 'BDC_OKCODE'                     ''    '=MAST',
   ' ' 'RALT_DYNP_STRUC-ANBTR01(01)'      wa_checkfield-anbtr1_1
                                             it_arq_b-anbtr1_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR01(16)'      wa_checkfield-anbtr1_6
                                             it_arq_b-anbtr1_6,

   ' ' 'RALT_DYNP_STRUC-ANBTR02(01)'      wa_checkfield-anbtr2_1
                                             it_arq_b-anbtr2_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR02(16)'      wa_checkfield-anbtr2_6
                                             it_arq_b-anbtr2_6,
   ' ' 'RALT_DYNP_STRUC-ANBTR03(01)'      wa_checkfield-anbtr3_1
                                             it_arq_b-anbtr3_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR03(16)'      wa_checkfield-anbtr3_6
                                             it_arq_b-anbtr3_6,
   ' ' 'RALT_DYNP_STRUC-ANBTR04(01)'      wa_checkfield-anbtr4_1
                                             it_arq_b-anbtr4_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR04(16)'      wa_checkfield-anbtr4_6
                                             it_arq_b-anbtr4_6,
   ' ' 'RALT_DYNP_STRUC-ANBTR05(01)'      wa_checkfield-anbtr5_1
                                             it_arq_b-anbtr5_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR05(16)'      wa_checkfield-anbtr5_6
                                             it_arq_b-anbtr5_6,
   ' ' 'RALT_DYNP_STRUC-ANBTR06(01)'      wa_checkfield-anbtr6_1
                                             it_arq_b-anbtr6_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR06(16)'      wa_checkfield-anbtr6_6
                                             it_arq_b-anbtr6_6,
   ' ' 'RALT_DYNP_STRUC-ANBTR07(01)'      wa_checkfield-anbtr7_1
                                             it_arq_b-anbtr7_1,
   ' ' 'RALT_DYNP_STRUC-ANBTR07(16)'      wa_checkfield-anbtr7_6
                                             it_arq_b-anbtr7_6.

      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
                   ' ' 'BDC_OKCODE'               ''      '=TAB01'.
      PERFORM f_bdc_insert USING: 'X' 'SAPLAIST'  ''      '1000',
               ' ' 'ANLA-AKTIV'   wa_checkfield-aktiv  it_arq_b-aktiv,
                   ' ' 'BDC_OKCODE'                     ''    '=BUCH'.


      CLEAR vg_ano.

    ELSE.

      PERFORM f_bdc_insert USING: 'X' 'SAPLALTD'  ''    '1100',
             ' ' 'BDC_OKCODE'                     ''    '=BUCH'.

      PERFORM f_calcula_valor USING      it_arq_b-anbtr1_1
                                         it_arq_b-anbtr1_6
                                         it_arq_b-ndjar1
                                         it_arq_b-ndper1
                                         ''
                               CHANGING  it_arq_b-anbtr1_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR01(01)'      wa_checkfield-anbtr1_1
                                                it_arq_b-anbtr1_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR01(06)'      wa_checkfield-anbtr1_6
                                                it_arq_b-anbtr1_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR01(16)'      wa_checkfield-anbtr1_6
                                                v_anbtr_16.

      PERFORM f_calcula_valor USING     it_arq_b-anbtr2_1
                                         it_arq_b-anbtr2_6
                                         it_arq_b-ndjar4
                                         it_arq_b-ndper4
                                         ''
                               CHANGING  it_arq_b-anbtr2_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR02(01)'      wa_checkfield-anbtr2_1
                                                it_arq_b-anbtr2_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR02(06)'      wa_checkfield-anbtr2_6
                                                it_arq_b-anbtr2_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR02(16)'      wa_checkfield-anbtr2_6
                                                v_anbtr_16.

      PERFORM f_calcula_valor USING      it_arq_b-anbtr3_1
                                         it_arq_b-anbtr3_6
                                         it_arq_b-ndjar1
                                         it_arq_b-ndper1
                                         ''
                               CHANGING  it_arq_b-anbtr3_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR03(01)'      wa_checkfield-anbtr3_1
                                                it_arq_b-anbtr3_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR03(06)'      wa_checkfield-anbtr3_6
                                                it_arq_b-anbtr3_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR03(16)'      wa_checkfield-anbtr3_6
                                                v_anbtr_16.

      PERFORM f_calcula_valor USING      it_arq_b-anbtr4_1
                                         it_arq_b-anbtr4_6
                                         it_arq_b-ndjar5
                                         it_arq_b-ndper5
                                         ''
                               CHANGING  it_arq_b-anbtr4_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR04(01)'      wa_checkfield-anbtr4_1
                                                it_arq_b-anbtr4_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR04(06)'      wa_checkfield-anbtr4_6
                                                it_arq_b-anbtr4_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR04(16)'      wa_checkfield-anbtr4_6
                                                v_anbtr_16.

      PERFORM f_calcula_valor USING      it_arq_b-anbtr5_1
                                         it_arq_b-anbtr5_6
                                         it_arq_b-ndjar3
                                         it_arq_b-ndper3
                                         wa_arquivo-afabg3
                               CHANGING  it_arq_b-anbtr5_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR05(01)'      wa_checkfield-anbtr5_1
                                                it_arq_b-anbtr5_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR05(06)'      wa_checkfield-anbtr5_6
                                                it_arq_b-anbtr5_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR05(16)'      wa_checkfield-anbtr5_6
                                                v_anbtr_16.

      PERFORM f_calcula_valor USING     it_arq_b-anbtr6_1
                                        it_arq_b-anbtr6_6
                                        it_arq_b-ndjar1
                                        it_arq_b-ndper1
                                        ''
                               CHANGING  it_arq_b-anbtr6_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR06(01)'      wa_checkfield-anbtr6_1
                                                it_arq_b-anbtr6_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR06(06)'      wa_checkfield-anbtr6_6
                                                it_arq_b-anbtr6_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR06(16)'      wa_checkfield-anbtr6_6
                                                v_anbtr_16.

      vg_flag = 'X'.
      PERFORM f_calcula_valor USING  it_arq_b-anbtr7_1
                                     it_arq_b-anbtr7_6
                                     it_arq_b-ndjar7
                                     it_arq_b-ndper7
                                     ''
                               CHANGING  it_arq_b-anbtr7_6.
      PERFORM f_bdc_insert USING:
      ' ' 'RALT_DYNP_STRUC-ANBTR07(01)'      wa_checkfield-anbtr7_1
                                                it_arq_b-anbtr7_1,
      ' ' 'RALT_DYNP_STRUC-ANBTR07(06)'      wa_checkfield-anbtr7_6
                                                it_arq_b-anbtr7_6,
      ' ' 'RALT_DYNP_STRUC-ANBTR07(16)'      wa_checkfield-anbtr7_6
                                                v_anbtr_16.
      CLEAR: vg_flag.
    ENDIF.

    IF NOT p_princ IS INITIAL.
      PERFORM bdc_transacao USING 'AS91'.
      IF sy-subrc EQ 0.
        MESSAGE s000
        WITH 'Pasta Batch-Input criada com sucesso. Ver SM35.'.
      ENDIF.
    ELSE.
      PERFORM bdc_transacao USING 'AS94'.

      IF sy-subrc EQ 0.
        MESSAGE s000
        WITH 'Pasta Batch-Input criada com sucesso.Ver SM35.'.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " bdc_processamento
*&---------------------------------------------------------------------*
*&      Form  f_calcula_data
*&---------------------------------------------------------------------*
*       Calcula número de dias
*----------------------------------------------------------------------*
FORM f_calcula_data .

  CLEAR: n_dias, n_ano, vg_data_ini, vg_dias, vg_year.

  vg_data_fim = '20070831'.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = wa_arquivo-aktiv
    IMPORTING
      date_internal            = vg_data_ini
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.



  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
    EXPORTING
      beg_da        = vg_data_ini
      end_da        = vg_data_fim
    IMPORTING
      no_day        = n_dias
      no_month      = n_mes
      no_year       = n_ano
      no_cal_day    = n_dias_tot
    EXCEPTIONS
      dateint_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    n_mes = n_mes * 30.
    n_dias = n_mes + n_dias.

    WRITE  n_dias TO vg_dias.
    CONDENSE vg_dias NO-GAPS.
    WRITE  n_ano  TO vg_year.
    CONDENSE vg_year NO-GAPS.
  ENDIF.


ENDFORM.                    " f_calcula_data
*&---------------------------------------------------------------------*
*&      Form  f_calcula_valor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_calcula_valor  USING    p_anbtr1   p_anbtr6
                               p_ndjar    p_ndper p_ini_depr
                      CHANGING p_anbtr_6.

  DATA: v_anbtr(15)   TYPE c,
        v_anbtr1(15)  TYPE c,
        v_anbtr_6(15) TYPE c,
        v_anbtr2      TYPE p DECIMALS 6,
        v_calc_util   TYPE p DECIMALS 6.

  DATA: v_depr_dia     TYPE p  DECIMALS 6,
        v_depr         TYPE p  DECIMALS 2,
        v_data2        TYPE d,
        v_depr_lancada TYPE p  DECIMALS 2,
        v_dias_a_depr  TYPE i,
        v_meses_2006   TYPE i,
        v_meses_2007   TYPE i.


  CLEAR: n_dias, n_ano, vg_data_ini, vg_dias, vg_year, v_anbtr,
         v_anbtr_6, n_dias_tot, v_util, v_anbtr2, v_depr_dia,  v_depr,
         v_data2, v_depr_lancada.


  vg_data_fim = '20070831'.

  v_anbtr = p_anbtr6.
  v_anbtr1 = p_anbtr1.
  v_anbtr_6 = p_anbtr_6.

  TRANSLATE  v_anbtr USING ',.'.
  TRANSLATE  v_anbtr_6 USING ',.'.
  TRANSLATE  v_anbtr1 USING ',.'.

  IF p_ini_depr IS INITIAL.
    PERFORM f_converte_data USING wa_arquivo-aktiv.
  ELSE.
    PERFORM f_converte_data USING p_ini_depr.
  ENDIF.

  IF  NOT rb_fore IS INITIAL.
    PERFORM f_calc_util USING wa_arquivo-anlkl p_ndjar p_ndper.
  ELSE.
    PERFORM f_calc_util USING it_arq_b-anlkl p_ndjar p_ndper.
  ENDIF.

  v_data2 = '20061231'.

  IF vg_data_ini(6) = '200612' AND
     v_data2(6)     = '200612'.
    v_meses_util = 1.
  ELSE.
    PERFORM f_calcula_mes USING vg_data_ini v_data2.
  ENDIF.


  IF v_util GT 0.
    v_depr_mes = v_anbtr1 / v_util.
    v_meses_2006 = v_meses_util.

    IF v_meses_util NE 0.
*    IF n_dias_tot GE v_util.
      IF v_anbtr1 = v_anbtr_6.
        TRANSLATE p_anbtr_6 USING '.,'.
        CONDENSE p_anbtr_6 NO-GAPS.
        CLEAR v_anbtr_16.
      ELSE.
        v_depr = v_depr_mes * v_meses_util.
        CLEAR: p_anbtr_6.

        MOVE  v_depr TO p_anbtr_6.
        TRANSLATE p_anbtr_6 USING '.,'.
        CONDENSE p_anbtr_6 NO-GAPS.
        vg_data_ini2 = '20070101'.
        PERFORM f_calcula_mes USING vg_data_ini2 vg_data_fim.
        v_meses_2007 = v_meses_util.
*      CLEAR: v_depr_mes.
*      v_depr_mes = v_anbtr_6 / v_util.
        v_depr_lancada = v_depr_mes * v_meses_util.
        CLEAR: v_anbtr_16.
*        WRITE v_depr_lancada  TO v_anbtr_16.
        MOVE v_depr_lancada  TO v_anbtr_16.
        TRANSLATE v_anbtr_16 USING '.,'.
        CONDENSE v_anbtr_16 NO-GAPS.

        DATA: v_valor_total TYPE p  DECIMALS 6,
              v_arred_menos TYPE p  DECIMALS 6,
              v_arred_mais  TYPE  p  DECIMALS 6.
        v_valor_total = v_depr + v_depr_lancada.

        v_arred_menos =  v_valor_total - 1.
        v_arred_mais  =  v_valor_total + 1.

        CLEAR: r_arred.
        FREE:  r_arred.
        r_arred-sign    = 'E'.
        r_arred-option  = 'BT'.
        r_arred-low     =  v_arred_menos .
        r_arred-high    =   v_arred_mais.
        APPEND r_arred.

*        IF v_valor_total NE v_anbtr_6.
        IF  v_anbtr_6 IN r_arred.

          IF vg_data_ini(6) = '200708' AND
             v_data2(6)     = '200708'.
            v_meses_util = 1.
          ELSE.
            PERFORM f_calcula_mes USING vg_data_ini vg_data_fim.
          ENDIF.

          CLEAR : v_depr_mes.

          IF v_meses_util GT 0.

            v_depr_mes = v_anbtr_6 / v_meses_util.
            v_depr = v_depr_mes * v_meses_2006.

            CLEAR: p_anbtr_6.
*            WRITE v_depr TO p_anbtr_6.
            MOVE v_depr TO p_anbtr_6.
            TRANSLATE p_anbtr_6 USING '.,'.
            CONDENSE p_anbtr_6 NO-GAPS.


            CLEAR: v_depr_lancada.

            v_depr_lancada = v_depr_mes * v_meses_2007.
            CLEAR: v_anbtr_16.
**            WRITE v_depr_lancada  TO v_anbtr_16.
            MOVE v_depr_lancada  TO v_anbtr_16.
            TRANSLATE v_anbtr_16 USING '.,'.
            CONDENSE v_anbtr_16 NO-GAPS.
          ELSE.
            CLEAR : p_anbtr1.
*            WRITE v_anbtr1 TO p_anbtr1.
            MOVE v_anbtr1 TO p_anbtr1.
            TRANSLATE p_anbtr1 USING '.,'.
            CONDENSE p_anbtr1 NO-GAPS.

            CLEAR v_anbtr_16.
            CLEAR p_anbtr_6.

          ENDIF.
        ENDIF.

      ENDIF.
    ELSE.
      CLEAR v_anbtr_16.
      CLEAR p_anbtr_6.
    ENDIF.

  ELSE.
    CLEAR: p_anbtr1.
*    WRITE v_anbtr1 TO p_anbtr1.
    MOVE v_anbtr1 TO p_anbtr1.
    TRANSLATE p_anbtr1 USING '.,'.
    CONDENSE p_anbtr1 NO-GAPS.

    CLEAR v_anbtr_16.
    CLEAR p_anbtr_6.
  ENDIF.

ENDFORM.                    " f_calcula_valor

*&---------------------------------------------------------------------*
*&      Form  f_calc_util
*&---------------------------------------------------------------------*

FORM f_calc_util  USING    p_anlkl p_ndjar p_ndper .

  CLEAR: v_util.

  IF p_ndjar IS INITIAL.
    IF vg_flag = 'X'.
      v_util = 4 * 360.
    ELSEIF     ( p_anlkl IN r_imob2 ) .
      v_util = 5 * 360.
    ELSEIF ( p_anlkl IN r_imob3 ) .
      v_util = 10 * 360.
    ELSEIF ( p_anlkl IN r_imob4 ) .
      v_util = 20 * 360.
    ELSEIF ( p_anlkl IN r_imob5 ) .
      v_util = 25 * 360.
    ENDIF.

  ELSE.

    v_util = p_ndjar * 12.
    v_util = v_util + p_ndper.

  ENDIF.

ENDFORM.                    " f_calc_util
*&---------------------------------------------------------------------*
*&      Form  f_converte_data
*&---------------------------------------------------------------------*

FORM f_converte_data  USING    p_data.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = p_data
    IMPORTING
      date_internal            = vg_data_ini
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.

ENDFORM.                    " f_converte_data
*&---------------------------------------------------------------------*
*&      Form  f_calcula_mes
*&---------------------------------------------------------------------*

FORM f_calcula_mes  USING p_data_ini  p_data_fim.

  CLEAR: v_meses_util, n_ano, n_mes, n_dias_tot.

  CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
    EXPORTING
      beg_da        = p_data_ini
      end_da        = p_data_fim
    IMPORTING
      no_day        = n_dias
      no_month      = n_mes
      no_year       = n_ano
      no_cal_day    = n_dias_tot
    EXCEPTIONS
      dateint_error = 1
      OTHERS        = 2.

  CLEAR : v_meses_util.

  IF sy-subrc EQ 0.

    v_meses_util =  n_ano * 12.
    v_meses_util = v_meses_util + n_mes.
  ENDIF.



ENDFORM.                    " f_calcula_mes
