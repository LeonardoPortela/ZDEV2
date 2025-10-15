*&---------------------------------------------------------------------*
*& Report  ZROMCONF
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zromconf.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

TABLES j_1bnfdoc .
TABLES vbfa.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF ty_remessa,
       seq_pla_romaneios TYPE zsdt0001-ch_referencia,
       remessa           TYPE lips-vbeln,
       ordem             TYPE lips-vgbel,
       nr_romaneio       TYPE zsdt0001-nr_romaneio,
       mblnr             TYPE mkpf-mblnr,
       mjahr             TYPE mkpf-mjahr,
       refkey            TYPE j_1bnflin-refkey,
      END OF ty_remessa,

      BEGIN OF ty_nf,
        refkey  TYPE j_1bnflin-refkey,
        docnum  TYPE j_1bnfdoc-docnum,
        series  TYPE j_1bnfdoc-series,
        nfenum  TYPE j_1bnfdoc-nfenum,
        menge   TYPE j_1bnflin-menge,
        netpr TYPE j_1bnflin-netpr,
      END OF ty_nf,

      BEGIN OF ty_saida,
        refkey      TYPE j_1bnflin-refkey,
        nfenum      TYPE j_1bnfdoc-nfenum, "Nr. Nota
        series      TYPE j_1bnfdoc-series, "sERIE
        menge       TYPE j_1bnflin-menge,  "Quantidade
        netwrt      TYPE j_1bnflin-netwrt,  "VALOR TOTAL
        docnum      TYPE j_1bnfdoc-docnum, "Nº documento
        ordem       TYPE lips-vgbel,
        remessa     TYPE lips-vbeln,
        romaneio    TYPE zsdt0001-nr_romaneio,
        ref_ro      TYPE zsdt0001-ch_referencia,
      END   OF ty_saida.



TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& CONSTANTES
*&---------------------------------------------------------------------*
CONSTANTS: c_pis(3)    TYPE c VALUE 'PIS',
           c_ipi(3)    TYPE c VALUE 'IPI',
           c_icms(4)   TYPE c VALUE 'ICMS',
           c_cofins(6) TYPE c VALUE 'COFI'.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: t_remessa   TYPE TABLE OF ty_remessa,
      t_nf        TYPE TABLE OF ty_nf,
      t_saida     TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: wa_remessa   TYPE ty_remessa,
      wa_nf        TYPE ty_nf,
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT re~ch_referencia AS seq_pla_romaneios  l~vbeln AS remessa  l~vgbel AS ordem  re~nr_romaneio v~mblnr v~mjahr
   FROM zsdt0001 AS re
  INNER JOIN lips AS l ON l~vbeln EQ re~doc_rem
  INNER JOIN mkpf AS v ON v~le_vbeln EQ l~vbeln
   INTO TABLE t_remessa
   where re~tp_movimento = 'S'.

  LOOP AT t_remessa INTO wa_remessa.
    CONCATENATE wa_remessa-mblnr wa_remessa-mjahr INTO wa_remessa-refkey.
    MODIFY t_remessa FROM wa_remessa INDEX sy-tabix TRANSPORTING refkey.
  ENDLOOP.

  SELECT nfl~refkey nf~docnum nf~series nf~nfenum nfl~menge nfl~netpr
    FROM j_1bnflin AS nfl
   INNER JOIN j_1bnfdoc AS nf ON nf~docnum  = nfl~docnum
    INTO TABLE t_nf
         FOR ALL ENTRIES IN t_remessa
         WHERE nfl~refkey  EQ t_remessa-refkey.
ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organiza_dados .
  DATA: nome_clifor TYPE c LENGTH 35,
        t_parid     TYPE c LENGTH 10,
        inscricao   TYPE c LENGTH 18,
        produto     TYPE c LENGTH 40,
        docnum      TYPE j_1bnfdoc-docnum,
        status      TYPE j_1bnfe_active-docsta,
        wa_kna1     TYPE kna1,
        wa_lfa1     TYPE lfa1,
        wa_t001w    TYPE t001w,
        it_item     TYPE TABLE OF j_1bnflin WITH HEADER LINE INITIAL SIZE 0,
        it_item_tax TYPE TABLE OF j_1bnfstx WITH HEADER LINE INITIAL SIZE 0.

  SORT: t_remessa   BY seq_pla_romaneios,
        t_nf        BY refkey.

  CLEAR t_saida.


  LOOP AT t_remessa INTO wa_remessa.

    READ TABLE t_nf INTO wa_nf
    WITH KEY refkey = wa_remessa-refkey
              BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      wa_saida-refkey      = wa_remessa-refkey.
      wa_saida-nfenum      = wa_nf-nfenum.
      wa_saida-series      = wa_nf-series.
      wa_saida-menge       = wa_nf-menge.
      wa_saida-netwrt      = wa_nf-menge * wa_nf-netpr.
      wa_saida-docnum      = wa_nf-docnum.
      wa_saida-ordem       = wa_remessa-ordem.
      wa_saida-remessa     = wa_remessa-remessa.
      wa_saida-romaneio    = wa_remessa-nr_romaneio.
      wa_saida-ref_ro      = wa_remessa-seq_pla_romaneios.
      APPEND wa_saida TO t_saida.
    ENDIF.
    CLEAR: wa_saida, wa_nf.
  ENDLOOP.
ENDFORM.                    " F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_montar_layout.
  PERFORM f_montar_estrutura USING:

   "TABELA "CAMPO    "TAB INTERNA  "VARIAVEL DA WA "CAPTION

  1  ''   ''      'T_SAIDA' 'REFKEY'    'refkey'                ' ',
  2  ''   ''      'T_SAIDA' 'NFENUM'    'nfenum'                ' ',
  3  ''   ''      'T_SAIDA' 'SERIES'    'series'        ' ',
  4  ''   ''      'T_SAIDA' 'MENGE'     'menge'            ' ',
  5  ''   ''      'T_SAIDA' 'NETWRT'    'netwrt'       ' ',
  6  ''   ''      'T_SAIDA' 'DOCNUM'    'docnum'       ' ',
  7  ''   ''      'T_SAIDA' 'ORDEM'     'ordem'                ' ',
  8  ''   ''      'T_SAIDA' 'REMESSA'   'remessa'  ' ',
  9  ''   ''      'T_SAIDA' 'ROMANEIO'  'romaneio'   ' ',
  10 ''   ''      'T_SAIDA' 'REF_RO'    'ref_ro'          ' '.
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

*  select single butxt from t001 into w_texto2
*    where bukrs eq p_bukrs.

*  concatenate 'Empresa:' p_bukrs '-' w_texto2 into w_texto1 separated by space.
*** Nome da empresa
*  perform f_construir_cabecalho using 'H' w_texto1.

*  select single name1 from t001w into w_texto2
*    where werks = p_branch.

*  concatenate 'Filial:' p_branch  '-' w_texto2 into  w_texto1 separated by space.
*  perform f_construir_cabecalho using 'S' w_texto1.

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
