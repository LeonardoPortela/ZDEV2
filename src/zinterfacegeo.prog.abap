*&---------------------------------------------------------------------*
*& Report  ZINTERFACEGEO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZINTERFACEGEO.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*

type-pools: slis.
tables : bkpf,MKPF.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

types:
      begin of ty_bkpf,
        belnr       type bkpf-belnr,
        awkey       type bkpf-awkey,
        bktxt       type bkpf-bktxt,
      end of ty_bkpf,

      begin of ty_bseg,
        belnr  type bseg-belnr,
        dmbtr  type bseg-dmbtr,
        dmbe2  type bseg-dmbe2,
        dmbe3  type bseg-dmbe3,
        matnr  type bseg-matnr,
        werks  type bseg-werks,
        menge  type bseg-menge,
        bewar    type bseg-bewar,
      end of ty_bseg,

      begin of ty_MKPF,
        MBLNR   type MKPF-MBLNR,
        mjahr   type MKPF-mjahr,
        DOC_MAT type C LENGTH 20,
      end   of ty_MKPF,

      begin of ty_MSEG,
       MBLNR TYPE MSEG-MBLNR,
       MJAHR TYPE MSEG-MJAHR,
       BWART TYPE MSEG-BWART,
       DOC_MAT type C LENGTH 20,
      END   of ty_MSEG,

      begin of ty_MSEG_2,
        DOC_MAT type C LENGTH 20,
        BWART TYPE MSEG-BWART,
      end   of ty_MSEG_2,

      begin of ty_saida,
        belnr    type bseg-belnr,
        dmbtr    type bseg-dmbtr,
        dmbe2    type bseg-dmbe2,
        dmbe3    type bseg-dmbe3,
        matnr    type bseg-matnr,
        awkey    type bkpf-awkey,
        bktxt    type bkpf-bktxt,
        werks    type bseg-werks,
        menge    type bseg-menge,
        BWART    type mseg-BWART,
        user     type sy-uname,
        data(25) type c,
      end   of ty_saida.


types: begin of ty_estrutura.
include type slis_fieldcat_main.
include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

data: t_bkpf   type table of ty_bkpf,
      t_bseg   type table of ty_bseg,
      t_MKPF   type table of ty_MKPF,
      t_MSEG   type table of ty_MSEG,
      t_MSEG_2 type table of ty_MSEG_2,

      t_saida type table of ty_saida.


*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

data: wa_bkpf   type ty_bkpf,
      wa_bseg   type ty_bseg,
      wa_MKPF   type ty_MKPF,
      WA_MSEG   type ty_MSEG,
      WA_MSEG_2 type ty_MSEG_2,
      wa_saida  type ty_saida.

*&---------------------------------------------------------------------*
*& VARIAVEIS AUX
*&---------------------------------------------------------------------*

data: x_data type d,
      x_hora type sy-uzeit.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

data: xs_events    type slis_alv_event,
      events       type slis_t_event,
      t_print      type slis_print_alv,
      estrutura    type table of ty_estrutura,
      wa_estrutura type ty_estrutura,
      v_report     like sy-repid,
      t_top        type slis_t_listheader,
      t_sort       type slis_t_sortinfo_alv with header line.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

selection-screen: begin of block b1 with frame title text-001.
       parameter: p_bukrs type bkpf-bukrs obligatory,
                  p_gjahr type bkpf-gjahr obligatory.
  select-options: p_budat for bkpf-budat no-extension,
                  p_awkey for bkpf-awkey,
                  p_mat   for MKPF-BKTXT.

selection-screen: end of block b1.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
perform f_iniciar_variaves.
perform f_seleciona_dados.
perform f_organiza_dados.
perform f_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*

form f_seleciona_dados .

 DATA : MATERIAL TYPE C LENGTH 14.

  if p_mat is initial and p_awkey is initial.
    message i000(z01) with 'Informar Texto do Item ou a Referência !'.
    stop.
  endif.


  IF NOT p_mat IS INITIAL.

    SELECT MBLNR mjahr mjahr
      into table t_MKPF
      FROM MKPF
      WHERE BKTXT IN p_mat.

    SELECT MBLNR MJAHR BWART
      FROM MSEG
      INTO TABLE t_MSEG
      FOR ALL ENTRIES IN t_MKPF
      WHERE MBLNR EQ t_MKPF-MBLNR
        AND MJAHR EQ t_MKPF-MJAHR .

*---> 04/07/2023 - Migração S4 - WS
  SORT t_MSEG.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM t_MSEG COMPARING ALL FIELDS.

    SORT t_MSEG BY MBLNR MJAHR.

    LOOP AT t_MKPF INTO WA_MKPF.

      CONCATENATE WA_MKPF-mblnr WA_MKPF-mjahr INTO WA_MKPF-DOC_MAT.

      modify t_MKPF from WA_MKPF index sy-tabix transporting DOC_MAT .

    ENDLOOP.


    LOOP AT t_MSEG INTO WA_MSEG.

      CONCATENATE WA_MSEG-mblnr WA_MSEG-mjahr INTO WA_MSEG-DOC_MAT.

      modify t_MSEG from WA_MSEG index sy-tabix transporting DOC_MAT .

    ENDLOOP.

    sort t_MKPF by DOC_MAT.

    SELECT belnr awkey bktxt
    FROM bkpf
      INTO table t_bkpf
      FOR ALL ENTRIES IN T_MKPF
    WHERE awkey EQ T_MKPF-DOC_MAT
      AND bukrs eq p_bukrs"(empresa)
      AND gjahr eq p_gjahr"(exercicio)
      AND budat in p_budat"(data)
      AND tcode in ('MFBF','MB1A', 'MF41')
      AND BLART = 'WA'
      AND awtyp = 'MKPF'.

  ELSE.
  " Selecionando os documentos contábeis baseados nos documentos de material:
    SELECT belnr awkey bktxt
      INTO table t_bkpf
      FROM bkpf
      WHERE bukrs eq p_bukrs"(empresa)
        AND gjahr eq p_gjahr"(exercicio)
        AND budat in p_budat"(data)
        AND tcode in ('MFBF','MB1A', 'MF41')
        AND awtyp = 'MKPF'
        AND BLART = 'WA'
        AND awkey in p_awkey  ."(documento de material||exercicio).
  ENDIF.

  IF sy-subrc is initial.
"Recuperando os valores reais, dolar, ufir e material
     DATA ETL222C4R2262 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L222C4R9557 TYPE FAGL_T_FIELD.
LT_FIELDS_L222C4R9557 = VALUE #( ( LINE = 'BELNR' )
 ( LINE = 'DMBTR' )
 ( LINE = 'DMBE2' )
 ( LINE = 'DMBE3' )
 ( LINE = 'MATNR' )
 ( LINE = 'WERKS' )
 ( LINE = 'MENGE' )
 ( LINE = 'BEWAR' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = T_BKPF
              I_WHERE_CLAUSE = |BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND BUKRS = { CL_ABAP_DYN_PRG=>QUOTE( P_BUKRS ) } AND GJAHR = '{ P_GJAHR }' AND BUZID = 'M' OR BUZID = 'S' AND SHKZG = 'H'|
              IT_FIELDLIST = LT_FIELDS_L222C4R9557
    IMPORTING ET_BSEG = ETL222C4R2262
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL222C4R2262 ) > 0.
  MOVE-CORRESPONDING ETL222C4R2262 TO T_BSEG.
  SY-DBCNT = LINES( ETL222C4R2262 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.
"(‘H’).
  ENDIF.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
form f_organiza_dados .

  sort: t_bkpf by belnr,
        t_bseg by belnr matnr dmbtr dmbe2 dmbe3,
        t_MSEG BY DOC_MAT.

  LOOP AT t_bkpf INTO WA_bkpf.


    loop at t_bseg into wa_bseg
      WHERE belnr EQ WA_bkpf-belnr.

      READ TABLE t_MSEG INTO WA_MSEG WITH KEY DOC_MAT = WA_bkpf-AWKEY BINARY SEARCH.
*    loop at t_mseg into wa_mseg
*      where DOC_MAT = WA_bkpf-AWKEY .
*
*      READ TABLE T_BSEG INTO WA_BSEG WITH KEY belnr = WA_bkpf-belnr BINARY SEARCH.


      wa_saida-awkey = WA_bkpf-awkey.
      wa_saida-belnr = wa_bseg-belnr.
      wa_saida-matnr = wa_bseg-matnr.
      wa_saida-werks = wa_bseg-werks.
      wa_saida-bktxt = wa_bkpf-bktxt.
      wa_saida-BWART = WA_MSEG-BWART.

      IF wa_saida-BWART EQ '261'.
        wa_saida-dmbtr = wa_bseg-dmbtr * -1.
        wa_saida-dmbe2 = wa_bseg-dmbe2 * -1.
        wa_saida-menge = wa_bseg-menge * -1.
        wa_saida-dmbe3 = wa_bseg-dmbe3 * -1.
      ELSE.
        wa_saida-dmbtr = wa_bseg-dmbtr.
        wa_saida-dmbe2 = wa_bseg-dmbe2.
        wa_saida-menge = wa_bseg-menge.
        wa_saida-dmbe3 = wa_bseg-dmbe3.
      ENDIF.

      x_data = sy-datum.
      x_hora = sy-uzeit.

      concatenate x_data+6(2) '/'
                x_data+4(2) '/'
                x_data(4)   ' -  '
                x_hora(2)   ':'
                x_hora+2(2) ':'
                x_hora+4(2) into wa_saida-data.

      wa_saida-user        = sy-uname.


      append wa_saida to t_saida.

      clear: wa_saida, wa_bseg, x_data, x_hora.
    endloop.
    clear: WA_bkpf.
  endloop.

endform.                    " F_ORGANIZA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*

form f_imprime_dados .
  if t_saida[] is initial.
    message i000(z01) with 'Não foram encontrados dados para os parametros'
                           'informados' .
    stop.
  endif.
  perform f_definir_eventos.
  perform f_alv_sort.
  perform f_montar_layout.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = estrutura[]
      it_sort                 = t_sort[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    tables
      t_outtab                = t_saida.

endform.                    " F_IMPRIME_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*

form f_definir_eventos .
  perform f_carregar_eventos using:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
endform.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*

form f_carregar_eventos using    name form.
  clear xs_events.
  xs_events-name = name.
  xs_events-form = form.
  append xs_events to events.
endform.                    " f_carregar_eventos

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*

form f_montar_layout.
  perform f_montar_estrutura using:

  'BELNR'  'Documento Contábil' ' ',
  'MENGE'  'Quantidade '        ' ',
  'DMBTR'  'Vr. Real'           ' ',
  'DMBE2'  'Vr.Dolar'           ' ',
  'DMBE3'  'Ufir'               ' ',
  'BWART'  'Tipo Movimento'     ' ',
  'MATNR'  'Material'           'X',
  'AWKEY'  'Doc Material'       ' ',
  'BKTXT'  'Txt. Cabecalho'     ' ',
  'WERKS'  'Centro'             ' '.


endform.                    " F_MONTAR_LAYOUT
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
form f_montar_estrutura using value(p_field)         like dd03d-fieldname
                              value(p_scrtext_l)     like dd03p-scrtext_l
                              value(p_zero)          type c.

  data: x_contador type string.
  clear: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = 'T_SAIDA'.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-no_zero       = p_zero                            .
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.


  if p_field eq 'DOCNUM'.
    wa_estrutura-hotspot = 'X'.
  else.
    clear wa_estrutura-hotspot.
  endif.

  append wa_estrutura to estrutura.

endform.                    " F_montar_estrutura

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xtop_of_page.                                          "#EC CALLED

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = t_top.
*            I_LOGO             = ''.

endform. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_iniciar_variaves.

  data: w_texto1(40).
  data: w_texto2(20).

  v_report = sy-repid.

*** Nome do Report
  perform f_construir_cabecalho using 'H' text-002.

  select single butxt from t001 into w_texto2
    where bukrs eq p_bukrs.

  concatenate 'Empresa:' p_bukrs '-' w_texto2 into w_texto1 separated by space.
*** Nome da empresa
  perform f_construir_cabecalho using 'H' w_texto1.

  write: sy-datum to w_texto2.
  concatenate 'Data:' w_texto2 into w_texto1 separated by space.

  perform f_construir_cabecalho using 'S' w_texto1.
  write: sy-uzeit to w_texto2.

  concatenate 'Hora:' w_texto2 into w_texto1 separated by space.
  perform f_construir_cabecalho using 'S' w_texto1.

  concatenate 'Usuário:' sy-uname into w_texto1 separated by space.
  perform f_construir_cabecalho using 'S' w_texto1.
endform.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
form f_construir_cabecalho using typ text.

  data: ls_line type slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  append ls_line to t_top.


*  CLEAR LS_LINE.
*  LS_LINE-TYP  = 'A'.
*  LS_LINE-KEY = 'QUEBRA'.
*  LS_LINE-INFO = ' '.
*  APPEND LS_LINE TO T_TOP.



endform.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_alv_sort.

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
endform.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
form f_user_command using l_ucomm
                          l_selfield type slis_selfield.

  if l_selfield-fieldname = 'DOCNUM'.
    read table t_saida index l_selfield-tabindex into wa_saida.

    set parameter id 'JEF' field l_selfield-value.

    call transaction 'J1B3N' and skip first screen.
    "CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

  endif.

endform.                    "f_user_command
