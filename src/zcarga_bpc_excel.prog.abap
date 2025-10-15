*&---------------------------------------------------------------------*
*& Report  ZCARGA_BPC_EXCEL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcarga_bpc_excel.

TYPE-POOLS ole2.

TABLES :bsis.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_carga,
    bukrs      TYPE bsis-bukrs,
    tp_moeda   TYPE c LENGTH 2,
    cd_moeda   TYPE c LENGTH 3,
    exec_per   TYPE c LENGTH 6,
    pl_contas  TYPE c LENGTH 4,
    hkont      TYPE bsis-hkont,
    vbund      TYPE bsis-vbund,
    kostl      TYPE bsis-kostl,
    bewar      TYPE bsis-bewar,
    mov_mensal TYPE P LENGTH 16 DECIMALS 2,
    sld_acum   TYPE P LENGTH 16 DECIMALS 2,
    area_cont  TYPE c LENGTH 4,
  END OF ty_carga,

  BEGIN OF ty_bsis,
    belnr TYPE bsis-belnr,
    buzei TYPE bsis-buzei,
    dmbtr TYPE bsis-dmbtr,
    dmbe2 TYPE bsis-dmbe2,
    dmbe3 TYPE bsas-dmbe3,
    kostl TYPE bsis-kostl,
    vbund TYPE bsis-vbund,
    bewar TYPE bsis-bewar,
    hkont TYPE bsis-hkont,
    bukrs TYPE bsis-bukrs,
    gjahr TYPE bsis-gjahr,
    monat TYPE bsis-monat,
    shkzg TYPE bsis-shkzg,
  END   OF ty_bsis,

  BEGIN OF ty_bsas,
    belnr TYPE bsas-belnr,
    buzei TYPE bsas-buzei,
    dmbtr TYPE bsas-dmbtr,
    dmbe2 TYPE bsas-dmbe2,
    dmbe3 TYPE bsas-dmbe3,
    kostl TYPE bsas-kostl,
    vbund TYPE bsas-vbund,
    bewar TYPE bsas-bewar,
    hkont TYPE bsas-hkont,
    bukrs TYPE bsas-bukrs,
    gjahr TYPE bsas-gjahr,
    monat TYPE bsas-monat,
    shkzg TYPE bsas-shkzg,
  END   OF ty_bsas.

*----------------------------------------------------------------------*
*  Tabelas internas
*----------------------------------------------------------------------*
DATA : it_carga TYPE TABLE OF ty_carga,
       it_bsis  TYPE TABLE OF ty_bsis,
       it_bsas  TYPE TABLE OF ty_bsas,
       it_merge TYPE TABLE OF ty_bsas.
*----------------------------------------------------------------------*
*  Work Areas
*----------------------------------------------------------------------*
DATA : wa_carga TYPE ty_carga,
       wa_bsas  TYPE ty_bsas,
       wa_merge TYPE ty_bsas.

*----------------------------------------------------------------------*
*  Declarações de variáveis
*----------------------------------------------------------------------*
DATA : linha   TYPE i,      " Atribui Valor Linha
       coluna  TYPE i,
       v_texto TYPE string . " Conteudo das celulas

*----------------------------------------------------------------------*
*  Definições de objetos OLE2
*----------------------------------------------------------------------*
DATA: gs_excel    TYPE ole2_object,       " Objeto Excel
      gs_workbook TYPE ole2_object,       " Workbook 'Area de trabalho'
      gs_sheet    TYPE ole2_object,       " Planilha
      gs_cell1    TYPE ole2_object,                     " Celula 1
      gs_cell2    TYPE ole2_object,                     " Celula 2
      gs_cells    TYPE ole2_object,       " Células
      gs_range    TYPE ole2_object,       " Grupo de células
      gs_font     TYPE ole2_object,       " Fonte da célula
      gs_column   TYPE ole2_object.       " Coluna da célula

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_bukrs FOR bsis-bukrs OBLIGATORY,
                p_hkont	FOR bsis-hkont OBLIGATORY,
                p_monat	FOR bsis-monat,
                p_budat FOR bsis-budat OBLIGATORY,
                p_gjahr	FOR bsis-gjahr OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.




START-OF-SELECTION.

  PERFORM : seleciona_dados,
            saida.

  IF it_carga IS NOT INITIAL.

    PERFORM: f_gera_excel,
             gera_cabecalho,
             gera_linhas,
             encerra_planilha_excel.

  ELSE.

    MESSAGE 'Não foi encontrado dados na tabela conforme críterios da tela de seleção'
      TYPE 'I'.

  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_GERA_EXCEL
*&---------------------------------------------------------------------*
FORM f_gera_excel .

  CREATE OBJECT gs_excel 'Excel.Application'.

  SET PROPERTY OF gs_excel 'ScreenUpdating'   = 0.
  SET PROPERTY OF gs_excel 'Visible'   = 1.

  GET PROPERTY OF gs_excel 'Workbooks' = gs_workbook.

  CALL METHOD OF gs_workbook 'Add'.
  PERFORM err_hdl.

  CALL METHOD OF gs_excel 'Worksheets' = gs_sheet.

  CALL METHOD OF gs_sheet 'Add' = gs_sheet.
  SET PROPERTY OF gs_sheet 'Name' = 'ABA Nova'.

  PERFORM err_hdl.

ENDFORM.                    " F_GERA_EXCEL
*&---------------------------------------------------------------------*
*&      Form  ERR_HDL
*&---------------------------------------------------------------------*
FORM err_hdl .

  IF sy-subrc <> 0.
    " Erro ....
    WRITE: / 'Erro na abertura OLE-Automation(EXCEL):', sy-subrc.
    STOP.
  ENDIF.

ENDFORM.                    " ERR_HDL

*&---------------------------------------------------------------------*
*&      Form  FORMATA_COLUNAS
*&---------------------------------------------------------------------*
FORM formata_colunas .

  PERFORM formata_largura USING 'E:E' '33'.

ENDFORM.                    " FORMATA_COLUNAS

*&---------------------------------------------------------------------*
*&      Form  FORMATA_LARGURA
*&---------------------------------------------------------------------*
FORM formata_largura  USING  coluna
                             largura.
  CALL METHOD OF gs_excel 'Range' = gs_range
    EXPORTING
    #1 = coluna.

  SET PROPERTY OF gs_range 'ColumnWidth' = largura.


ENDFORM.                    " FORMATA_LARGURA

*&---------------------------------------------------------------------*
*&      Form  GERA_CABECALHO
*&---------------------------------------------------------------------*
FORM gera_cabecalho .
  linha  = 1.
  coluna = 1.

  v_texto = 'Empresa'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Tipo de moeda'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Código da moeda'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Exercício/período'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Plano de contas'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Conta do Razão'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Tipo de movimento'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Sociedade Parceira'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Área de contabilidade de custos'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Centro de custo'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Grupo de mercadoria'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Movimentação mensal'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

  v_texto = 'Saldo acumulado'.
  PERFORM preenche_titulo USING linha coluna linha coluna  v_texto.
  coluna = coluna + 1.

ENDFORM.                    " GERA_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_TITULO
*&---------------------------------------------------------------------*
FORM preenche_titulo  USING linha1
                            coluna1
                            linha2
                            coluna2
                            texto.

* Mescla células selecionadas
  PERFORM seleciona_celula USING linha1 coluna1 gs_cell1.
* Preenche célula
  PERFORM preenche_celula USING gs_cell1 1 3 0 12 texto.


ENDFORM.                    " PREENCHE_TITULO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CELULA
*&---------------------------------------------------------------------*
FORM seleciona_celula  USING    linha
                                coluna
                                celula.

* Seleciona célula
  CALL METHOD OF gs_excel 'Cells' = celula
    EXPORTING
    #1 = linha
    #2 = coluna.

ENDFORM.                    " SELECIONA_CELULA

*&---------------------------------------------------------------------*
*&      Form  MESCLA_CELULAS
*&---------------------------------------------------------------------*
FORM mescla_celulas .

* Passa que é um intervalo entre estes dois campos.
  CALL METHOD OF gs_excel 'Range' = gs_cells
    EXPORTING
    #1 = gs_cell1
    #2 = gs_cell2.

* Mescla células do intevalos acima.
  CALL METHOD OF gs_cells 'Merge' = gs_cells.

ENDFORM.                    " MESCLA_CELULAS
*&---------------------------------------------------------------------*
*&      Form  CRIA_BORDA
*&---------------------------------------------------------------------*
FORM cria_borda  USING    celula1
                          celula2
                          celula.

* Seleciona o intervalo de células
  CALL METHOD OF gs_excel 'Range' = celula
    EXPORTING
    #1 = celula1
    #2 = celula2.

* Cria borda e os valores de exportação são cor e intensidade.
  CALL METHOD OF celula 'BorderAround'
    EXPORTING
      #1 = 1
      #2 = 3.


ENDFORM.                    " CRIA_BORDA

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CELULA
*&---------------------------------------------------------------------*
FORM preenche_celula  USING celula
                            bold
                            alinhamento
                            largura
                            tam
                            valor.

* Formata célula
  "GET PROPERTY OF celula  'Font' = gs_font.
  "SET PROPERTY OF gs_font 'Bold' = bold.
  "SET PROPERTY OF gs_font 'Size' = tam.
  "SET PROPERTY OF celula  'HorizontalAlignment' = alinhamento.
  "SET PROPERTY OF celula  'NumberFormat' = '@'.
* Preenche célula
  SET PROPERTY OF celula  'Value' = valor.


ENDFORM.                    " PREENCHE_CELULA

*&---------------------------------------------------------------------*
*&      Form  SELECTS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  REFRESH : it_bsis, it_bsas, it_merge.

  SELECT belnr
         buzei
         dmbtr
         dmbe2
         dmbe3
         kostl
         vbund
         bewar
         hkont
         bukrs
         gjahr
         monat
         shkzg
    FROM bsis
    INTO TABLE it_bsis
   WHERE bukrs IN p_bukrs
     AND hkont IN p_hkont
     AND gjahr IN p_gjahr
     AND monat IN p_monat
     AND budat IN p_budat
     AND bstat NE 'S'.

  SELECT belnr
         buzei
         dmbtr
         dmbe2
         dmbe3
         kostl
         vbund
         bewar
         hkont
         bukrs
         gjahr
         monat
         shkzg
    FROM bsas
    INTO TABLE it_bsas
   WHERE bukrs IN p_bukrs
     AND hkont IN p_hkont
     AND gjahr IN p_gjahr
     AND monat IN p_monat
     AND budat IN p_budat
     AND bstat NE 'S'.

ENDFORM.                    " SELECTS


*&---------------------------------------------------------------------*
*&      Form  SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saida .

  DATA: vl_bukrs     TYPE bsas-bukrs,
        vl_vbund     TYPE bsas-vbund,
        vl_kostl     TYPE bsas-kostl,
        vl_bewar     TYPE bsas-bewar,
        vl_hkont     TYPE bsas-hkont,
        vl_dmbtr     TYPE bsas-dmbtr,
        vl_dmbe2     TYPE bsas-dmbe2,
        vl_dmbe3     TYPE bsas-dmbe3,
        vl_exercicio TYPE c LENGTH 6.

  vl_exercicio = p_budat-high(6).

  it_merge[] = it_bsis[].

  LOOP AT it_bsas INTO wa_bsas .
    APPEND wa_bsas TO it_merge .
  ENDLOOP.

  SORT it_merge BY bukrs vbund hkont kostl  bewar.

  CLEAR : vl_bukrs, vl_vbund, vl_hkont, vl_kostl, vl_bewar.

  vl_dmbtr = 0.
  vl_dmbe2 = 0.
  vl_dmbe3 = 0.

  LOOP AT it_merge INTO wa_merge .

    IF vl_bukrs = wa_merge-bukrs AND
       vl_vbund = wa_merge-vbund AND
       vl_kostl = wa_merge-kostl AND
       vl_hkont = wa_merge-hkont AND
       vl_bewar = wa_merge-bewar .

      IF wa_merge-shkzg = 'H'.
        vl_dmbtr = vl_dmbtr + ( wa_merge-dmbtr * -1 ).
        vl_dmbe2 = vl_dmbe2 + ( wa_merge-dmbe2 * -1 ).
        vl_dmbe3 = vl_dmbe3 + ( wa_merge-dmbe3 * -1 ).
      ELSE.
        vl_dmbtr = vl_dmbtr + wa_merge-dmbtr.
        vl_dmbe2 = vl_dmbe2 + wa_merge-dmbe2.
        vl_dmbe3 = vl_dmbe3 + wa_merge-dmbe3.
      ENDIF.

    ELSE.
      CLEAR : wa_carga.

      IF vl_bukrs NE '' .

        IF ( vl_bukrs NE '0100') AND ( vl_bukrs NE '0101').

          "MOEDA = BRL
          wa_carga-bukrs      = vl_bukrs.
          wa_carga-tp_moeda   = '10'.
          wa_carga-cd_moeda   = 'BRL'.
          wa_carga-exec_per   = vl_exercicio.
          wa_carga-pl_contas  = '0050'.
          wa_carga-hkont      = vl_hkont.
          wa_carga-vbund      = vl_vbund.
          wa_carga-kostl      = vl_kostl.
          wa_carga-bewar      = vl_bewar.
          wa_carga-mov_mensal = vl_dmbtr.
          wa_carga-sld_acum   = vl_dmbtr.
          IF vl_bukrs EQ '0035' OR vl_bukrs EQ '0038'.
            wa_carga-area_cont = 'MGLD'.
          ELSE.
            wa_carga-area_cont = 'MAGI'.
          ENDIF.

          APPEND wa_carga TO it_carga.
          CLEAR : wa_carga.

        ELSEIF vl_bukrs EQ '0101'.
          wa_carga-bukrs      = vl_bukrs.
          wa_carga-tp_moeda   = '10'.
          wa_carga-cd_moeda   = 'PGY'.
          wa_carga-exec_per   = vl_exercicio.
          wa_carga-pl_contas  = '0050'.
          wa_carga-hkont      = vl_hkont.
          wa_carga-vbund      = vl_vbund.
          wa_carga-kostl      = vl_kostl.
          wa_carga-bewar      = vl_bewar.
          wa_carga-mov_mensal = ( vl_dmbtr * 100 ).
          wa_carga-sld_acum   = ( vl_dmbtr * 100 ).
          wa_carga-area_cont = 'MGPY'.

          APPEND wa_carga TO it_carga.
          CLEAR : wa_carga.

          "MOEDA = BRL
          wa_carga-bukrs      = vl_bukrs.
          wa_carga-tp_moeda   = '30'.
          wa_carga-cd_moeda   = 'BRL'.
          wa_carga-exec_per   = vl_exercicio.
          wa_carga-pl_contas  = '0050'.
          wa_carga-hkont      = vl_hkont.
          wa_carga-vbund      = vl_vbund.
          wa_carga-kostl      = vl_kostl.
          wa_carga-bewar      = vl_bewar.
          wa_carga-mov_mensal = vl_dmbe3.
          wa_carga-sld_acum   = vl_dmbe3.
          wa_carga-area_cont  = 'MGPY'.

          APPEND wa_carga TO it_carga.
          CLEAR : wa_carga.

        ELSE.

          "MOEDA = ARS
          wa_carga-bukrs      = vl_bukrs.
          wa_carga-tp_moeda   = '10'.
          wa_carga-cd_moeda   = 'ARS'.
          wa_carga-exec_per   = vl_exercicio.
          wa_carga-pl_contas  = '0050'.
          wa_carga-hkont      = vl_hkont.
          wa_carga-vbund      = vl_vbund.
          wa_carga-kostl      = vl_kostl.
          wa_carga-bewar      = vl_bewar.
          wa_carga-mov_mensal = vl_dmbtr.
          wa_carga-sld_acum   = vl_dmbtr.
          wa_carga-area_cont = 'MGAR'.

          APPEND wa_carga TO it_carga.
          CLEAR : wa_carga.

          "MOEDA = BRL
          wa_carga-bukrs      = vl_bukrs.
          wa_carga-tp_moeda   = '30'.
          wa_carga-cd_moeda   = 'BRL'.
          wa_carga-exec_per   = vl_exercicio.
          wa_carga-pl_contas  = '0050'.
          wa_carga-hkont      = vl_hkont.
          wa_carga-vbund      = vl_vbund.
          wa_carga-kostl      = vl_kostl.
          wa_carga-bewar      = vl_bewar.
          wa_carga-mov_mensal = vl_dmbe3.
          wa_carga-sld_acum   = vl_dmbe3.
          wa_carga-area_cont = 'MGAR'.

          APPEND wa_carga TO it_carga.
          CLEAR : wa_carga.
        ENDIF.

        "MOEDA = USD
        wa_carga-bukrs       = vl_bukrs.
        wa_carga-tp_moeda    = '40'.
        wa_carga-cd_moeda    = 'USD'.
        wa_carga-exec_per    = vl_exercicio.
        wa_carga-pl_contas   = '0050'.
        wa_carga-hkont       = vl_hkont.
        wa_carga-vbund       = vl_vbund.
        wa_carga-kostl       = vl_kostl.
        wa_carga-bewar       = vl_bewar.
        wa_carga-mov_mensal  = vl_dmbe2.
        wa_carga-sld_acum    = vl_dmbe2.

        IF vl_bukrs EQ '0035' OR vl_bukrs EQ '0038'.
          wa_carga-area_cont = 'MGLD'.
        ELSEIF vl_bukrs EQ '0100' .
          wa_carga-area_cont = 'MGAR'.
        ELSEIF vl_bukrs EQ '0101' .
          wa_carga-area_cont = 'MGPY'.
        ELSE.
          wa_carga-area_cont = 'MAGI'.
        ENDIF.

        APPEND wa_carga TO it_carga.

      ENDIF.

      vl_bukrs = wa_merge-bukrs.
      vl_vbund = wa_merge-vbund.
      vl_kostl = wa_merge-kostl.
      vl_bewar = wa_merge-bewar.
      vl_hkont = wa_merge-hkont.

      IF wa_merge-shkzg = 'H'.
        vl_dmbtr = ( wa_merge-dmbtr * -1 ).
        vl_dmbe2 = ( wa_merge-dmbe2 * -1 ).
        vl_dmbe3 = ( wa_merge-dmbe3 * -1 ).
      ELSE.
        vl_dmbtr = wa_merge-dmbtr.
        vl_dmbe2 = wa_merge-dmbe2.
        vl_dmbe3 = wa_merge-dmbe3.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF ( vl_bukrs NE '0100') AND ( vl_bukrs NE '0101').

    "MOEDA = BRL
    wa_carga-bukrs      = vl_bukrs.
    wa_carga-tp_moeda   = '10'.
    wa_carga-cd_moeda   = 'BRL'.
    wa_carga-exec_per   = vl_exercicio.
    wa_carga-pl_contas  = '0050'.
    wa_carga-hkont      = vl_hkont.
    wa_carga-vbund      = vl_vbund.
    wa_carga-kostl      = vl_kostl.
    wa_carga-bewar      = vl_bewar.
    wa_carga-mov_mensal = vl_dmbtr.
    wa_carga-sld_acum   = vl_dmbtr.
    IF vl_bukrs EQ '0035' OR vl_bukrs EQ '0038'.
      wa_carga-area_cont = 'MGLD'.
    ELSE.
      wa_carga-area_cont = 'MAGI'.
    ENDIF.

    APPEND wa_carga TO it_carga.
    CLEAR : wa_carga.

  ELSEIF vl_bukrs EQ '0101'.
    wa_carga-bukrs      = vl_bukrs.
    wa_carga-tp_moeda   = '10'.
    wa_carga-cd_moeda   = 'PGY'.
    wa_carga-exec_per   = vl_exercicio.
    wa_carga-pl_contas  = '0050'.
    wa_carga-hkont      = vl_hkont.
    wa_carga-vbund      = vl_vbund.
    wa_carga-kostl      = vl_kostl.
    wa_carga-bewar      = vl_bewar.
    wa_carga-mov_mensal = ( vl_dmbtr * 100 ).
    wa_carga-sld_acum   = ( vl_dmbtr * 100 ).
    wa_carga-area_cont = 'MGPY'.

    APPEND wa_carga TO it_carga.
    CLEAR : wa_carga.

    "MOEDA = BRL
    wa_carga-bukrs      = vl_bukrs.
    wa_carga-tp_moeda   = '30'.
    wa_carga-cd_moeda   = 'BRL'.
    wa_carga-exec_per   = vl_exercicio.
    wa_carga-pl_contas  = '0050'.
    wa_carga-hkont      = vl_hkont.
    wa_carga-vbund      = vl_vbund.
    wa_carga-kostl      = vl_kostl.
    wa_carga-bewar      = vl_bewar.
    wa_carga-mov_mensal = vl_dmbe3.
    wa_carga-sld_acum   = vl_dmbe3.
    wa_carga-area_cont = 'MGPY'.

    APPEND wa_carga TO it_carga.
    CLEAR : wa_carga.

  ELSE.
    "MOEDA = ARS
    wa_carga-bukrs      = vl_bukrs.
    wa_carga-tp_moeda   = '10'.
    wa_carga-cd_moeda   = 'ARS'.
    wa_carga-exec_per   = vl_exercicio.
    wa_carga-pl_contas  = '0050'.
    wa_carga-hkont      = vl_hkont.
    wa_carga-vbund      = vl_vbund.
    wa_carga-kostl      = vl_kostl.
    wa_carga-bewar      = vl_bewar.
    wa_carga-mov_mensal = vl_dmbtr.
    wa_carga-sld_acum   = vl_dmbtr.
    wa_carga-area_cont = 'MGAR'.

    APPEND wa_carga TO it_carga.
    CLEAR : wa_carga.

    "MOEDA = BRL
    wa_carga-bukrs      = vl_bukrs.
    wa_carga-tp_moeda   = '30'.
    wa_carga-cd_moeda   = 'BRL'.
    wa_carga-exec_per   = vl_exercicio.
    wa_carga-pl_contas  = '0050'.
    wa_carga-hkont      = vl_hkont.
    wa_carga-vbund      = vl_vbund.
    wa_carga-kostl      = vl_kostl.
    wa_carga-bewar      = vl_bewar.
    wa_carga-mov_mensal = vl_dmbe3.
    wa_carga-sld_acum   = vl_dmbe3.
    wa_carga-area_cont = 'MGAR'.

    APPEND wa_carga TO it_carga.
    CLEAR : wa_carga.
  ENDIF.

  "MOEDA = USD
  wa_carga-bukrs       = vl_bukrs.
  wa_carga-tp_moeda    = '40'.
  wa_carga-cd_moeda    = 'USD'.
  wa_carga-exec_per    = vl_exercicio.
  wa_carga-pl_contas   = '0050'.
  wa_carga-hkont       = vl_hkont.
  wa_carga-vbund       = vl_vbund.
  wa_carga-kostl       = vl_kostl.
  wa_carga-bewar       = vl_bewar.
  wa_carga-mov_mensal  = vl_dmbe2.
  wa_carga-sld_acum    = vl_dmbe2.

  IF vl_bukrs EQ '0035' OR vl_bukrs EQ '0038'.
    wa_carga-area_cont = 'MGLD'.
  ELSEIF vl_bukrs EQ '0100' .
    wa_carga-area_cont = 'MGAR'.
  ELSEIF vl_bukrs EQ '0101' .
    wa_carga-area_cont = 'MGPY'.
  ELSE.
    wa_carga-area_cont = 'MAGI'.
  ENDIF.

  APPEND wa_carga TO it_carga.

ENDFORM.                    " SAIDA

*&---------------------------------------------------------------------*
*&      Form  MONTA_LINHAS
*&---------------------------------------------------------------------*
FORM monta_linhas .

  PERFORM preenche_detalhe USING linha 1 0 0 7 3 wa_carga-bukrs.
  PERFORM preenche_detalhe USING linha 2 0 0 7 3 wa_carga-tp_moeda.
  PERFORM preenche_detalhe USING linha 3 0 0 7 3 wa_carga-cd_moeda.
  PERFORM preenche_detalhe USING linha 4 0 0 7 3 wa_carga-exec_per.
  PERFORM preenche_detalhe USING linha 5 0 0 7 3 wa_carga-pl_contas.
  PERFORM preenche_detalhe USING linha 6 0 0 7 3 wa_carga-hkont.
  PERFORM preenche_detalhe USING linha 7 0 0 7 3 wa_carga-bewar.
  PERFORM preenche_detalhe USING linha 8 0 0 7 3 wa_carga-vbund.
  PERFORM preenche_detalhe USING linha 9 0 0 7 3 wa_carga-area_cont.
  PERFORM preenche_detalhe USING linha 10 0 0 7 3 wa_carga-kostl.
  PERFORM preenche_detalhe USING linha 11 0 0 7 3 ''.
  PERFORM preenche_detalhe USING linha 12 0 0 7 3 wa_carga-mov_mensal.
  PERFORM preenche_detalhe USING linha 13 0 0 7 3 wa_carga-sld_acum .

ENDFORM.                    " MONTA_LINHAS

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DETALHE
*&---------------------------------------------------------------------*
FORM preenche_detalhe  USING linha
                             coluna
                             bold
                             largura
                             tam
                             alinhamento
                             conteudo.

* Seleciona célula
  PERFORM seleciona_celula USING linha coluna gs_cell1.

* Preenche célula
  PERFORM preenche_celula USING gs_cell1 bold alinhamento
                                largura tam conteudo.

ENDFORM.                    " PREENCHE_DETALHE

*&---------------------------------------------------------------------*
*&      Form  GERA_LINHAS
*&---------------------------------------------------------------------*
FORM gera_linhas .

  LOOP AT it_carga INTO wa_carga.
    linha = linha + 1.
    PERFORM monta_linhas.

  ENDLOOP.

ENDFORM.                    " GERA_LINHAS

*&---------------------------------------------------------------------*
*&      Form  ENCERRA_PLANILHA_EXCEL
*&---------------------------------------------------------------------*
FORM encerra_planilha_excel .

  " Faz com que a planilha só aparece quando terminar de ser alimentada.
  SET PROPERTY OF gs_excel 'ScreenUpdating'   = 1.

  " Libera os cara aee.
  FREE OBJECT gs_sheet.
  FREE OBJECT gs_workbook.
  FREE OBJECT gs_excel.


ENDFORM.                    " ENCERRA_PLANILHA_EXCEL
