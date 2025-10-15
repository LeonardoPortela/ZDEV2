*&---------------------------------------------------------------------*
*& Report  ZCONFFRETE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zconffrete.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
* TIPOS PARA ALV
*----------------------------------------------------------------------*

TYPE-POOLS: slis.

TABLES : vfkp, VTTK.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

TYPES:
      BEGIN OF ty_vttk,
        werks   TYPE vfkp-werks,
        erdat   TYPE vttk-erdat,
        tknum   TYPE vttk-tknum,
        fknum   TYPE vfkp-fknum,
        KNUMV   TYPE vfkp-KNUMV,
        RECHS   type vfkp-RECHS,
        FKSTO   type vfkp-FKSTO,
        SHTYP   type VTTK-SHTYP,
        TDLNR   TYPE VTTK-TDLNR,
        EXTI1   TYPE VTTK-EXTI1,
      END   OF ty_vttk,

      BEGIN OF ty_konv,
        knumv  LIKE konv-knumv,
        kwert  LIKE konv-kwert,
        KSCHL  LIKE konv-KSCHL,
      END   OF ty_konv,

      BEGIN OF ty_lfa1,
        lifnr     TYPE lfa1-lifnr,
        name1     TYPE lfa1-name1,
      END   OF ty_lfa1,

      BEGIN OF ty_saida,
        cod_prest   TYPE lfa1-lifnr,
        prestador   TYPE lfa1-name1,
        pis         TYPE lfa1-stenr,
        werks       TYPE vfkp-werks,
        erdat       TYPE vttk-erdat,
        tknum       TYPE vttk-tknum,
        fknum       TYPE vfkp-fknum,
        VALOR_CTRC  TYPE VTRLP-NETWR,
        VAL_INSS    TYPE KONV-kwert,
        VAL_SEST    TYPE KONV-kwert,
        VAL_IRRF    TYPE KONV-kwert,
        EXTI1       TYPE VTTK-EXTI1,
        TDLNR       TYPE VTTK-TDLNR,
        SHTYP       TYPE C LENGTH 27,
        data(25)    TYPE c,
        user        TYPE sy-uname,
      END   OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: t_vttk      TYPE TABLE OF ty_vttk,
      t_lfa1      TYPE TABLE OF ty_lfa1,
      t_saida     TYPE TABLE OF ty_saida,
      t_konv      TYPE TABLE OF ty_konv.


*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_vttk      TYPE ty_vttk,
      wa_j_1bnfstx TYPE j_1bnfstx,
      wa_konv      TYPE ty_konv,
      wa_saida     TYPE ty_saida.

*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

DATA: x_data TYPE d,
      x_hora TYPE sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_sort       TYPE slis_t_sortinfo_alv WITH HEADER LINE.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
     PARAMETER: p_bukrs  TYPE vfkp-bukrs OBLIGATORY,
                P_TDLNR  TYPE VTTK-TDLNR OBLIGATORY.
SELECT-OPTIONS: P_erdat  FOR VTTK-erdat OBLIGATORY NO-EXTENSION,
                p_werks  for vfkp-werks no intervals NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
PERFORM f_iniciar_variaves.
PERFORM f_seleciona_dados.
PERFORM f_organiza_dados.
PERFORM f_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

FORM f_seleciona_dados .

  SELECT vf~werks
         vt~erdat
         vt~tknum
         vf~fknum
         VF~KNUMV
         vf~RECHS
         vf~FKSTO
         VT~SHTYP
         VT~TDLNR
         VT~EXTI1
    INTO TABLE t_vttk
    FROM vttk AS vt
    INNER JOIN vttp AS vp ON vp~tknum EQ vt~tknum
    INNER JOIN vfkp AS vf ON vf~rebel EQ vt~tknum
    WHERE VF~BUKRS eq p_bukrs
      and vt~TDLNR eq P_TDLNR
      AND vf~werks in p_werks
      AND vt~erdat IN P_erdat
      and vt~add02 eq '0000000003'. "pessoa física


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM f_organiza_dados .

  DATA: BEGIN OF it_vbpavb.
          INCLUDE STRUCTURE vbpavb.
  DATA: END OF it_vbpavb.

  DATA: BEGIN OF it_VTRLP.
          INCLUDE STRUCTURE VTRLP.
  DATA: END OF it_VTRLP.

  DATA: wa_lfa1    TYPE lfa1,
        wa_vbpavb  type vbpavb,
        wa_VTRLP   type VTRLP,
        VAL_CTRC   TYPE KONP-PKWRT,
        TP_TRANSP  TYPE TVTKT-BEZEI,
        t_vbpavb   LIKE STANDARD TABLE OF it_vbpavb,
        t_VTRLP    LIKE STANDARD TABLE OF it_VTRLP.

  SORT: t_vttk BY tknum.

  LOOP AT t_vttk INTO wa_vttk.

    CLEAR wa_lfa1.

    CALL FUNCTION 'RV_SHIPMENT_VIEW'
    EXPORTING
      shipment_number                   = wa_vttk-tknum
      OPTION_TVTK                       = 'X'
      language                          = sy-langu
      option_partners                   = 'X'
      OPTION_PACKAGES                   = 'X'
      activity                          = 'A'
      option_no_refresh                 = 'X'
      i_filter_type                     = 'F'
    TABLES
      F_TRLP                            = T_VTRLP
      f_vbpa                            = t_vbpavb.

    delete t_vbpavb where parvw ne 'PV'.

    READ TABLE t_vbpavb INTO wa_vbpavb index 1.

    select single *
      from lfa1
      into wa_lfa1
      where lifnr eq wa_vbpavb-LIFNR.

    VAL_CTRC = 0.

    SELECT SINGLE NETWR
      INTO VAL_CTRC
      FROM VFSI
      WHERE KNUMV EQ wa_vttk-KNUMV
        AND ACTIV EQ 'X'.

    TRY.

CL_PRC_RESULT_FACTORY=>GET_INSTANCE( )->GET_PRC_RESULT( )->GET_PRICE_ELEMENT_DB(
  EXPORTING IT_SELECTION_ATTRIBUTE = VALUE #(
 ( fieldname = 'KNUMV' value = WA_VTTK-KNUMV )
 )
  IMPORTING ET_PRC_ELEMENT_CLASSIC_FORMAT = DATA(ETL217C4R6350) ).
  CLEAR T_KONV.
  TYPES: BEGIN OF TYL217C4R5177,
    KNUMV TYPE KONV-KNUMV,
    KWERT TYPE KONV-KWERT,
    KSCHL TYPE KONV-KSCHL,
  END OF TYL217C4R5177.
  DATA: LML217C4R4971 TYPE TYL217C4R5177,
        LWL217C4R135 LIKE LINE OF T_KONV.
  LOOP AT ETL217C4R6350 REFERENCE INTO DATA(LDRL217C4R3608).
    LML217C4R4971-KNUMV = LDRL217C4R3608->KNUMV.
    LML217C4R4971-KWERT = LDRL217C4R3608->KWERT.
    LML217C4R4971-KSCHL = LDRL217C4R3608->KSCHL.
    LWL217C4R135 = LML217C4R4971.
    APPEND LWL217C4R135 TO T_KONV.
  ENDLOOP.
CATCH CX_PRC_RESULT .
  SY-SUBRC = 4.
ENDTRY.

    LOOP AT T_KONV INTO WA_KONV.
      IF WA_KONV-KSCHL EQ 'ZINS'.
        WA_SAIDA-VAL_INSS = WA_KONV-kwert.
      ELSEIF WA_KONV-KSCHL EQ 'ZIRF'.
        WA_SAIDA-VAL_IRRF = WA_KONV-kwert.
      ELSEIF WA_KONV-KSCHL EQ 'ZSET'.
        WA_SAIDA-VAL_SEST = WA_KONV-kwert.
      ENDIF.

    ENDLOOP.

    SELECT SINGLE BEZEI
      INTO TP_TRANSP
      FROM TVTKT
      WHERE SHTYP EQ wa_vttk-SHTYP.

    wa_saida-EXTI1       = wa_vttk-EXTI1.
    wa_saida-TDLNR       = wa_vttk-TDLNR.
    wa_saida-VALOR_CTRC  = VAL_CTRC.
    wa_saida-prestador   = wa_lfa1-name1.
    wa_saida-pis         = wa_lfa1-stenr.
    wa_saida-cod_prest   = wa_lfa1-lifnr.
    wa_saida-werks       = wa_vttk-werks.
    wa_saida-erdat       = wa_vttk-erdat.
    wa_saida-tknum       = wa_vttk-tknum.
    wa_saida-fknum       = wa_vttk-fknum.
    concatenate wa_vttk-SHTYP  ' - ' TP_TRANSP(20) into wa_saida-SHTYP.
    x_data = sy-datum.
    x_hora = sy-uzeit.

    CONCATENATE x_data+6(2) '/'
                x_data+4(2) '/'
                x_data(4)   ' -  '
                x_hora(2)   ':'
                x_hora+2(2) ':'
                x_hora+4(2) INTO wa_saida-data.

    wa_saida-user        = sy-uname.

    APPEND wa_saida TO t_saida.

    CLEAR: wa_saida, wa_lfa1, wa_vttk, wa_konv, t_vbpavb, t_VTRLP, wa_VTRLP, wa_vbpavb, x_data, x_hora.

  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

FORM f_imprime_dados .
  IF t_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.

  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    TABLES
      t_outtab                = t_saida.

ENDFORM.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*

FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*

FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

FORM f_montar_layout.
  PERFORM f_montar_estrutura USING:

   1 ''   ''      'T_SAIDA' 'WERKS'        'Centro'                 ' ',
   2 ''   ''      'T_SAIDA' 'TDLNR'        'Agente de Frete'        ' ',
   3 ''   ''      'T_SAIDA' 'ERDAT'        'DATA'                   ' ',
   4 ''   ''      'T_SAIDA' 'TKNUM'        'Doc. Transporte'        ' ',
   5 ''   ''      'T_SAIDA' 'FKNUM'        'Nº CST.FRETE'           ' ',
   6 ''   ''      'T_SAIDA' 'EXTI1'        'Nº do DACTE'            ' ',
   7 ''   ''      'T_SAIDA' 'PIS'          'PIS/INSS'               ' ',
   8 ''   ''      'T_SAIDA' 'COD_PREST'    'CÓD SAP'                ' ',
   9 ''   ''      'T_SAIDA' 'PRESTADOR'    'NOME DO PRESTADOR'      ' ',
  10 ''   ''      'T_SAIDA' 'VALOR_CTRC'   'VALOR DO CTRC'          ' ',
  11 ''   ''      'T_SAIDA' 'VAL_INSS'     'INSS'                   ' ',
  12 ''   ''      'T_SAIDA' 'VAL_SEST'     'SEST/SENAT'             ' ',
  13 ''   ''      'T_SAIDA' 'VAL_IRRF'     'IRFF'                   ' ',
  14 ''   ''      'T_SAIDA' 'SHTYP'        'Tp CSTS Frete'          ' '.

ENDFORM.                    " F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM f_montar_estrutura USING value(p_col_pos)       TYPE i
                              value(p_ref_tabname)   LIKE dd02d-tabname
                              value(p_ref_fieldname) LIKE dd03d-fieldname
                              value(p_tabname)       LIKE dd02d-tabname
                              value(p_field)         LIKE dd03d-fieldname
                              value(p_scrtext_l)     LIKE dd03p-scrtext_l
                              value(p_outputlen).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = STRLEN( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.


  IF p_field EQ 'DOCNUM'.
    wa_estrutura-hotspot = 'X'.
  ELSE.
    CLEAR wa_estrutura-hotspot.
  ENDIF.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  DATA: w_texto1(40).
  DATA: w_texto2(20).

  v_report = sy-repid.

*** Nome do Report
  PERFORM f_construir_cabecalho USING 'H' text-002.

  SELECT SINGLE butxt FROM t001 INTO w_texto2
    WHERE bukrs EQ p_bukrs.

  CONCATENATE 'Empresa:' p_bukrs '-' w_texto2 INTO w_texto1 SEPARATED BY space.
*** Nome da empresa
  PERFORM f_construir_cabecalho USING 'H' w_texto1.

  IF NOT p_werks IS INITIAL.

    SELECT SINGLE name1 FROM t001w INTO w_texto2
      WHERE werks = p_werks.

    CONCATENATE 'Filial:' p_werks  '-' w_texto2 INTO  w_texto1 SEPARATED BY space.
    PERFORM f_construir_cabecalho USING 'S' w_texto1.
  ENDIF.

  WRITE: sy-datum TO w_texto2.
  CONCATENATE 'Data:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  WRITE: sy-uzeit TO w_texto2.
  CONCATENATE 'Hora:' w_texto2 INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
  CONCATENATE 'Usuário:' sy-uname INTO w_texto1 SEPARATED BY space.
  PERFORM f_construir_cabecalho USING 'S' w_texto1.
ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.



ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort.

*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUKRS'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 1.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'BUTXT'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 2.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'GSBER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 3.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'NAME'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 4.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'DATA'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 5.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
*
*  CLEAR T_SORT.
*  T_SORT-FIELDNAME = 'USER'.
*  T_SORT-SUBTOT    = 'X'.
*  T_SORT-SPOS      = 6.
*  T_SORT-UP        = 'X'.
*  APPEND T_SORT.
ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  IF l_selfield-fieldname = 'DOCNUM'.
    READ TABLE t_saida INDEX l_selfield-tabindex INTO wa_saida.

    SET PARAMETER ID 'JEF' FIELD l_selfield-value.

    CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
    "CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  ENDIF.

ENDFORM.                    "f_user_command
