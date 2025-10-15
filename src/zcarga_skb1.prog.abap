*&---------------------------------------------------------------------*
*& Report ZCARGA_SKB1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcarga_skb1.

*----------------------------------------------------------*
* Declarando TYPE-POOLS
*----------------------------------------------------------*
TYPE-POOLS: truxs.
*----------------------------------------------------------*
* Declarando tabela interna
*----------------------------------------------------------*
DATA: it_raw TYPE truxs_t_text_data.
*----------------------------------------------------------*
* Estrutura TYPES
*----------------------------------------------------------*
TYPES: BEGIN OF ty_arquivo,
         col01 TYPE string,
         col02 TYPE string,
         col03 TYPE string,
         col04 TYPE string,
         col05 TYPE string,
         col06 TYPE string,
         col07 TYPE string,
         col08 TYPE string,
         col09 TYPE string,
         col10 TYPE string,
         col11 TYPE string,
         col12 TYPE string,
         col13 TYPE string,
         col14 TYPE string,
         col15 TYPE string,
         col16 TYPE string,
         col17 TYPE string,
         col18 TYPE string,
         col19 TYPE string,
         col20 TYPE string,
         col21 TYPE string,
         col22 TYPE string,
         col23 TYPE string,
         col24 TYPE string,
         col25 TYPE string,
         col26 TYPE string,
         col27 TYPE string,
         col28 TYPE string,
         col29 TYPE string,
         col30 TYPE string,
         col31 TYPE string,
         col32 TYPE string,
         col33 TYPE string,
         col34 TYPE string,
         col35 TYPE string,
         col36 TYPE string,
         col37 TYPE string,
         col38 TYPE string,
         col39 TYPE string,
         col40 TYPE string,
         col41 TYPE string,
         col42 TYPE string,
         col43 TYPE string,
         col44 TYPE string,
         col45 TYPE string,
         col46 TYPE string,
         col47 TYPE string,
         col48 TYPE string.
*        include TYPE skb1.
TYPES:  END OF ty_arquivo.
*----------------------------------------------------------*
* Tabela(s) Interna(s)
*----------------------------------------------------------*
DATA: t_file TYPE TABLE OF ty_arquivo,
      w_file TYPE ty_arquivo,
      w_skb1 TYPE skb1.

*----------------------------------------------------------*
* Seleção de arquivo
*----------------------------------------------------------*
PARAMETER p_file TYPE rlgrap-filename.
*----------------------------------------------------------*
* Evento que irá manipular ações do usuário ao pressionar F4
*----------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  PERFORM f_busca_arquivo.
*----------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------*
  PERFORM f_upload.
*----------------------------------------------------------*
* Chamando método que irá buscar o arquivo no diretério local
*----------------------------------------------------------*
FORM f_busca_arquivo.
*----------------------------------------------------------*
* Declarando tabelas e wa para trabalhar com File Open Dialog
*----------------------------------------------------------*
  DATA: lt_filetable TYPE         filetable,
        ls_filetable TYPE LINE OF filetable,
        l_rc         TYPE i.

  CLEAR: ls_filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Selecione o arquivo para Upload'
      default_extension       = 'XLSX'
      file_filter             = 'Arquivos do Excel (*.XLS)|*.XLS| Excel files (*.XLSX)|*.XLSX|'
*     initial_directory       = desktop_directory  " Definir um local específico para procurar o arquivo
    CHANGING
      file_table              = lt_filetable
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
* Identifica o arquivo selecionado e joga para o parametro de seleção     READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    p_file = ls_filetable-filename.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD
*&---------------------------------------------------------------------*
*       Fazendo upload do arquivo importado para ser tratado internamente
*----------------------------------------------------------------------*
FORM f_upload .

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = it_raw            " WORK TABLE
      i_filename           = p_file
    TABLES
      i_tab_converted_data = t_file  " TABELA INTERNA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT t_file INTO w_file.
    w_skb1-bukrs = w_file-col02.
    w_skb1-saknr = w_file-col03.
    w_skb1-begru = w_file-col04.
    w_skb1-busab = w_file-col05.
    w_skb1-fdgrv = w_file-col09.
    w_skb1-fdlev = w_file-col10.
    w_skb1-fipls = w_file-col11.
    w_skb1-fstag = w_file-col12.
    w_skb1-hbkid = w_file-col13.
    w_skb1-hktid = w_file-col14.
    w_skb1-kdfsl = w_file-col15.
    w_skb1-mitkz = w_file-col16.
    w_skb1-mwskz = w_file-col17.
    w_skb1-stext = w_file-col18.
    w_skb1-vzskz = w_file-col19.
    w_skb1-waers = w_file-col20.
    w_skb1-wmeth = w_file-col21.
    w_skb1-xgkon = w_file-col22.
    w_skb1-xintb = w_file-col23.
    w_skb1-xkres = w_file-col24.
    w_skb1-xloeb = w_file-col25.
    w_skb1-xnkon = w_file-col26.
    w_skb1-xopvw = w_file-col27.
    w_skb1-xspeb = w_file-col28.
    w_skb1-zindt = w_file-col29.
    w_skb1-zinrt = w_file-col30.
    w_skb1-zuawa = w_file-col31.
    w_skb1-altkt = w_file-col32.
    w_skb1-xmitk = w_file-col33.
    w_skb1-recid = w_file-col34.
    w_skb1-fipos = w_file-col35.
    w_skb1-xmwno = w_file-col36.
    w_skb1-xsalh = w_file-col37.
    w_skb1-bewgp = w_file-col38.
    w_skb1-infky = w_file-col39.
    w_skb1-togru = w_file-col40.
    w_skb1-xlgclr = w_file-col41.
    w_skb1-x_uj_clr = w_file-col42.
    w_skb1-mcakey          = w_file-col43.
    w_skb1-cochanged       = w_file-col44.
    w_skb1-last_changed_ts = w_file-col45.

    UPDATE skb1 SET begru = w_skb1-begru
                    busab = w_skb1-begru
                    fdgrv = w_skb1-fdgrv
                    fdlev = w_skb1-fdlev
                    fipls = w_skb1-fipls
                    fstag = w_skb1-fstag
                    hbkid = w_skb1-hbkid
                    hktid = w_skb1-begru
                    kdfsl = w_skb1-kdfsl
                    mitkz = w_skb1-mitkz
                    mwskz = w_skb1-mwskz
                    stext = w_skb1-stext
                    vzskz = w_skb1-vzskz
                    waers = w_skb1-waers
                    wmeth = w_skb1-wmeth
                    xgkon = w_skb1-xgkon
                    xintb = w_skb1-xintb
                    xkres = w_skb1-xkres
                    xloeb = w_skb1-xloeb
                    xnkon = w_skb1-xnkon
                    xopvw = w_skb1-xopvw
                    xspeb = w_skb1-xspeb
                    zindt = w_skb1-zindt
                    zinrt = w_skb1-zinrt
                    zuawa = w_skb1-zuawa
                    altkt = w_skb1-altkt
                    xmitk = w_skb1-xmitk
                    recid = w_skb1-recid
                    fipos = w_skb1-fipos
                    xmwno = w_skb1-xmwno
                    xsalh = w_skb1-xsalh
                    bewgp = w_skb1-bewgp
                    infky = w_skb1-infky
                    togru = w_skb1-togru
                    xlgclr = w_skb1-xlgclr
                    x_uj_clr        = w_skb1-x_uj_clr
                    mcakey          = w_skb1-mcakey
                    cochanged       = w_skb1-cochanged
                    last_changed_ts = w_skb1-last_changed_ts
              WHERE bukrs = w_skb1-bukrs
                AND saknr = w_skb1-saknr.

    COMMIT WORK.

  ENDLOOP.

ENDFORM.
