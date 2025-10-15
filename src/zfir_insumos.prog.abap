*&---------------------------------------------------------------------*
*& Report  ZFIR_INSUMOS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir_insumos.

" Tabelas transparentes ....
TABLES : rbkp,rseg.

TYPE-POOLS: slis.

" Tipos .....
TYPES:BEGIN OF ty_lfa1,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
      END OF ty_lfa1.

TYPES: BEGIN OF ty_bkpf_log,
         bukrs    TYPE bkpf-bukrs,
         belnr    TYPE bkpf-belnr,
         gjahr    TYPE bkpf-gjahr,
         objectid TYPE cdhdr-objectid,
       END OF ty_bkpf_log.

TYPES: BEGIN OF ty_cdhdr,
         objectclas TYPE cdhdr-objectclas,
         objectid   TYPE cdhdr-objectid,
         changenr   TYPE cdhdr-changenr,
         username   TYPE cdhdr-username,
         udate      TYPE cdhdr-udate,
       END OF ty_cdhdr.

TYPES:BEGIN OF ty_cdpos,
        objectclas TYPE cdpos-objectclas,
        objectid   TYPE cdpos-objectid,
        changenr   TYPE cdpos-changenr,
        tabname    TYPE cdpos-tabname,
        fname      TYPE cdpos-fname,
        value_new  TYPE cdpos-value_new,
        value_old  TYPE cdpos-value_old,
      END OF ty_cdpos.

TYPES: BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey,
       END OF ty_bkpf.

TYPES:BEGIN OF ty_rseg,
        belnr TYPE rseg-belnr,
        gjahr TYPE rseg-gjahr,
        ebeln TYPE rseg-ebeln,
      END OF ty_rseg.

TYPES:BEGIN OF ty_rbkp,

        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        blart TYPE rbkp-blart,
        budat TYPE rbkp-budat,
        xblnr TYPE rbkp-xblnr,
        bukrs TYPE rbkp-bukrs,
        lifnr TYPE rbkp-lifnr,
        gsber TYPE rbkp-gsber,
      END OF ty_rbkp.

TYPES:BEGIN OF ty_rbkp_contabil,

        belnr    TYPE rbkp-belnr,
        gjahr    TYPE rbkp-gjahr,
        blart    TYPE rbkp-blart,
        budat    TYPE rbkp-budat,
        xblnr    TYPE rbkp-xblnr,
        bukrs    TYPE rbkp-bukrs,
        lifnr    TYPE rbkp-lifnr,
        gsber    TYPE rbkp-gsber,
        contabil TYPE bkpf-awkey,
      END OF ty_rbkp_contabil.

TYPES :

  BEGIN OF ty_saida,
    bukrs        TYPE rbkp-bukrs, "empresa
    gsber        TYPE rbkp-gsber, "centro
    ebeln        TYPE rseg-ebeln, "pedido
    lifnr        TYPE rbkp-lifnr, "forncedor
    name1        TYPE lfa1-name1, "nome fornecedor
    lifre        TYPE rbkp-lifnr, "codigo fornecedor fatura
    numero(20)   TYPE c, "numero miro
    budat        TYPE rbkp-budat, "data lançamento
    xblnr        TYPE rbkp-xblnr, "referencia
    contabil(20) TYPE c, "Documento contábil
    username     TYPE cdhdr-username, "Usuario Alteração
    udate        TYPE cdhdr-udate, "Data alteração
    fname        TYPE cdpos-fname, "Campo Alterado
    value_old    TYPE cdpos-value_old, "Valor antigo
    value_new    TYPE cdpos-value_new, "Valor novo
  END OF   ty_saida .


" Tabelas Internas ....
DATA : t_saida         TYPE TABLE OF ty_saida,
       t_rbkp          TYPE TABLE OF ty_rbkp,
       t_rbkp_contabil TYPE TABLE OF ty_rbkp_contabil,
       t_bkpf          TYPE TABLE OF ty_bkpf,
       t_lfa1          TYPE TABLE OF ty_lfa1,
       t_cdpos         TYPE TABLE OF ty_cdpos,
       t_cdhdr         TYPE TABLE OF ty_cdhdr,
       t_bkpf_log      TYPE TABLE OF ty_bkpf_log,
       t_rseg          TYPE TABLE OF ty_rseg.

"estruturas
DATA : it_fieldcat TYPE slis_t_fieldcat_alv,
       st_fieldcat TYPE slis_fieldcat_alv.

DATA: wa_rbkp          TYPE ty_rbkp,
      wa_bkpf          TYPE ty_bkpf,
      wa_bkpf_log      TYPE ty_bkpf_log,
      wa_cdhdr         TYPE ty_cdhdr,
      wa_lfa1          TYPE ty_lfa1,
      wa_cdpos         TYPE ty_cdpos,
      wa_saida         TYPE ty_saida,
      wa_rbkp_contabil TYPE ty_rbkp_contabil,
      wa_rseg          TYPE ty_rseg.

" Tela de seleção .....
SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS :
      so_bukrs FOR rbkp-bukrs OBLIGATORY,
      so_gsber FOR rbkp-gsber,
      so_budat FOR rbkp-budat OBLIGATORY,
      so_lifnr FOR rbkp-lifnr,
      so_xblnr FOR rbkp-xblnr,
      so_ebeln FOR rseg-ebeln.


SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM : zseleciona_dados,
            zmonta_saida.

  IF NOT t_saida IS INITIAL.
    PERFORM: zmonta_fieldcat,
             z_alv.

  ELSE.
    MESSAGE 'Sem dados a processar' TYPE 'S'.

  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  ZSELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zseleciona_dados .


  SELECT belnr
         gjahr
         blart
         budat
         xblnr
         bukrs
         lifnr
         gsber
         FROM rbkp
         INTO TABLE t_rbkp
         WHERE blart = 'IN'
           AND budat IN so_budat
           AND xblnr IN so_xblnr
           AND bukrs IN so_bukrs
           AND lifnr IN so_lifnr
           AND gsber IN so_gsber.

  IF sy-subrc = 0.

    SELECT lifnr
           name1
           FROM lfa1
           INTO TABLE t_lfa1
           FOR ALL ENTRIES IN t_rbkp
           WHERE lifnr = t_rbkp-lifnr.


    LOOP AT t_rbkp INTO wa_rbkp.

      wa_rbkp_contabil-belnr   = wa_rbkp-belnr.
      wa_rbkp_contabil-gjahr   = wa_rbkp-gjahr.
      wa_rbkp_contabil-blart   = wa_rbkp-blart.
      wa_rbkp_contabil-budat   = wa_rbkp-budat.
      wa_rbkp_contabil-xblnr   = wa_rbkp-xblnr.
      wa_rbkp_contabil-bukrs   = wa_rbkp-bukrs.
      wa_rbkp_contabil-lifnr   = wa_rbkp-lifnr.
      wa_rbkp_contabil-gsber   = wa_rbkp-gsber.

      CONCATENATE wa_rbkp-belnr wa_rbkp-gjahr INTO wa_rbkp_contabil-contabil.


      APPEND wa_rbkp_contabil  TO t_rbkp_contabil.
      CLEAR:wa_rbkp_contabil.


    ENDLOOP.

    SELECT   belnr
             gjahr
             ebeln
             FROM rseg
             INTO TABLE t_rseg
             FOR ALL ENTRIES IN t_rbkp
             WHERE belnr = t_rbkp-belnr
               AND gjahr = t_rbkp-gjahr
               AND ebeln IN so_ebeln.


    SELECT   bukrs
             belnr
             gjahr
             awkey
             FROM bkpf
             INTO TABLE t_bkpf
             FOR ALL ENTRIES IN t_rbkp_contabil
             WHERE awkey = t_rbkp_contabil-contabil.


    IF sy-subrc = 0.

      LOOP AT t_bkpf INTO wa_bkpf.

        wa_bkpf_log-bukrs = wa_bkpf-bukrs.
        wa_bkpf_log-belnr = wa_bkpf-belnr.
        wa_bkpf_log-gjahr = wa_bkpf-gjahr.
        CONCATENATE sy-mandt wa_bkpf-bukrs wa_bkpf-belnr wa_bkpf-gjahr INTO wa_bkpf_log-objectid.
        APPEND wa_bkpf_log TO t_bkpf_log.
        CLEAR:wa_bkpf_log.

      ENDLOOP.

      SELECT   objectclas
               objectid
               changenr
               username
               udate
               FROM cdhdr
               INTO TABLE t_cdhdr
               FOR ALL ENTRIES IN t_bkpf_log
               WHERE objectclas = 'BELEG'
                 AND objectid  = t_bkpf_log-objectid.

      SELECT   objectclas
               objectid
               changenr
               tabname
               fname
               value_new
               value_old
               FROM cdpos
               INTO TABLE t_cdpos
               FOR ALL ENTRIES IN t_bkpf_log
               WHERE objectclas = 'BELEG'
                 AND objectid  = t_bkpf_log-objectid.


    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZMONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zmonta_saida .

  DATA:dfies TYPE TABLE OF dfies.

  LOOP AT t_rbkp INTO wa_rbkp.


    wa_saida-bukrs  = wa_rbkp-bukrs.
    wa_saida-gsber  = wa_rbkp-gsber.
    wa_saida-lifnr  = wa_rbkp-lifnr.
    wa_saida-budat  = wa_rbkp-budat.
    wa_saida-xblnr  = wa_rbkp-xblnr.
    wa_saida-lifre  = wa_rbkp-lifnr.


    CLEAR:wa_lfa1.
    READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_rbkp-lifnr.

    IF sy-subrc = 0.

      wa_saida-name1 = wa_lfa1-name1.

    ENDIF.

    CLEAR:wa_rseg.
    READ TABLE t_rseg INTO wa_rseg WITH KEY  belnr = wa_rbkp-belnr
                                             gjahr = wa_rbkp-gjahr.

    IF sy-subrc = 0.

      wa_saida-ebeln  = wa_rseg-ebeln.

      CLEAR:wa_rbkp_contabil.
      READ TABLE t_rbkp_contabil INTO wa_rbkp_contabil WITH  KEY   belnr   = wa_rbkp-belnr
                                                                   gjahr   = wa_rbkp-gjahr
                                                                   blart   = wa_rbkp-blart
                                                                   budat   = wa_rbkp-budat
                                                                   xblnr   = wa_rbkp-xblnr
                                                                   bukrs   = wa_rbkp-bukrs
                                                                   lifnr   = wa_rbkp-lifnr
                                                                   gsber   = wa_rbkp-gsber.

      IF sy-subrc = 0.

        wa_saida-numero  = wa_rbkp-belnr."wa_rbkp_contabil-contabil.

        CLEAR:wa_bkpf.
        READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey = wa_rbkp_contabil-contabil.


        IF sy-subrc = 0.

*          CONCATENATE wa_bkpf-belnr wa_bkpf-gjahr INTO wa_saida-contabil.
          wa_saida-contabil  = wa_bkpf-belnr.

          CLEAR:wa_bkpf_log.
          READ TABLE t_bkpf_log INTO wa_bkpf_log WITH KEY bukrs = wa_bkpf-bukrs
                                                          belnr = wa_bkpf-belnr
                                                          gjahr = wa_bkpf-gjahr.

          IF sy-subrc = 0.




          ENDIF.

        ENDIF.

      ENDIF.


      CLEAR:wa_cdpos.
      LOOP AT t_cdhdr INTO wa_cdhdr WHERE objectid = wa_bkpf_log-objectid.
        wa_saida-username     = wa_cdhdr-username.
        wa_saida-udate        = wa_cdhdr-udate.
        LOOP AT t_cdpos INTO wa_cdpos WHERE objectid  = wa_cdhdr-objectid
                                      AND   changenr  = wa_cdhdr-changenr.


          CALL FUNCTION 'DDIF_FIELDINFO_GET'
            EXPORTING
              tabname   = wa_cdpos-tabname
              fieldname = wa_cdpos-fname
              langu     = sy-langu
            TABLES
              dfies_tab = dfies.


          READ TABLE dfies INTO DATA(ls_dfies) INDEX 1.

          IF sy-subrc = 0.
            wa_saida-fname        = ls_dfies-scrtext_l.

          ELSE.
            wa_saida-fname        = wa_cdpos-fname.

          ENDIF.
          wa_saida-value_old    = wa_cdpos-value_old.
          wa_saida-value_new    = wa_cdpos-value_new.
          APPEND wa_saida TO t_saida.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    CLEAR:wa_saida.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK       = ' '
*     I_BYPASSING_BUFFER      = ' '
*     I_BUFFER_ACTIVE         = ' '
      i_callback_program      = sy-repid    " Nome do programa
*     i_callback_pf_status_set          = ' '
      i_callback_user_command = 'USERCMND'
*     I_CALLBACK_TOP_OF_PAGE  = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
*     I_GRID_TITLE            =
*     I_GRID_SETTINGS         =
*     is_layout               =
      it_fieldcat             = it_fieldcat " catalogo de campos
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS       =
*     IT_SORT                 =
*     IT_FILTER               =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
*     i_save                  = 'X'
*     IS_VARIANT              =
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT                =
*     IS_REPREP_ID            =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       = 0
*     I_HTML_HEIGHT_END       = 0
*     IT_ALV_GRAPHICS         =
*     IT_HYPERLINK            =
*     IT_ADD_FIELDCAT         =
*     IT_EXCEPT_QINFO         =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = t_saida   " Tabela com os dados
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZMONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zmonta_fieldcat .

  PERFORM z_feed_fieldcat USING :
    "Fname       Seltext                 check   Edit  Col_pos
    'BUKRS'     'Empresa'                  ' '    ' '    0  ' ',
    'GSBER'     'Centro'                   ' '    ' '    1  ' ',
    'EBELN'     'Pedido'                   ' '    ' '    2  'X',
    'LIFNR'     'Fornecedor'               ' '    ' '    3  ' ',
    'NAME1'     'Nome Fornecedor'          ' '    ' '    4  ' ',
    'LIFRE'     'Código Fornecedor Fatura' ' '    ' '    5  ' ',
    'NUMERO'    'Número Miro'              ' '    ' '    6  'X',
    'BUDAT'     'Data de Lançamento'       ' '    ' '    7  ' ',
    'XBLNR'     'Referência'               ' '    ' '    8  ' ',
    'CONTABIL'  'Documento Contábil'       ' '    ' '    9  'X',
    'USERNAME'  'Usuário Alteração'        ' '    ' '    10 ' ',
    'UDATE'     'Data Alteração'           ' '    ' '    11 ' ',
    'FNAME'     'Campo Alterado'           ' '    ' '    12 ' ',
    'VALUE_OLD' 'Valor Antigo'             ' '    ' '    13 ' ',
    'VALUE_NEW' 'Valor Novo'               ' '    ' '    14 ' '.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_FEED_FIELDCAT
*&---------------------------------------------------------------------*
FORM z_feed_fieldcat  USING    fieldname
                               seltext_m
                               checkbox
                               edit
                               col_pos
                               hots.

  st_fieldcat-hotspot =  hots.
  st_fieldcat-fieldname   = fieldname.      " Nome do campo
  st_fieldcat-seltext_m   = seltext_m.      " texto do campo
  st_fieldcat-checkbox    = checkbox.       " se o campo é do tipo CheckBox
  st_fieldcat-edit        = edit.           " se o campo pode ser editavel
  st_fieldcat-col_pos     = col_pos.        "Em que posição fica a coluna
  " Coloca a estrutura alimentada na Tabela.
  APPEND st_fieldcat TO it_fieldcat.
  " Limpa a estrutura e volta pra poxima linha do form.
  CLEAR  st_fieldcat.
ENDFORM.                    " Z_FEED_FIELDCAT

FORM usercmnd USING r_ucomm LIKE sy-ucomm

rs_selfield TYPE slis_selfield .

  IF rs_selfield-sel_tab_field = '1-EBELN'."pedido

    SET PARAMETER ID 'BES' FIELD rs_selfield-value.
    CALL TRANSACTION 'ME23N'.

  ELSEIF rs_selfield-sel_tab_field =  '1-NUMERO'."numero miro

    SET PARAMETER ID 'RBN' FIELD rs_selfield-value.

    READ TABLE t_rbkp_contabil INTO DATA(ls_saida_hot) WITH  KEY belnr = rs_selfield-value.

    SET PARAMETER ID 'GJR' FIELD ls_saida_hot-gjahr.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  ELSEIF  rs_selfield-sel_tab_field =  '1-CONTABIL'."documento contabel

    SET PARAMETER ID 'BLN' FIELD rs_selfield-value.

    READ TABLE t_bkpf INTO DATA(ls_bkpf_hot) WITH KEY belnr = rs_selfield-value.
    SET PARAMETER ID 'BUK' FIELD ls_bkpf_hot-bukrs.

    SET PARAMETER ID 'GJR' FIELD ls_bkpf_hot-gjahr.

    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  ENDIF.


ENDFORM.
