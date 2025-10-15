FUNCTION-POOL z_gera_contrato.              "MESSAGE-ID ..

DATA: vl_gdatu TYPE gdatu_inv.

TYPES: BEGIN OF ty_0041,
         doc_simulacao TYPE zsded003,
         werks         TYPE werks,
         werks_fk      TYPE kna1-kunnr,
       END OF ty_0041.

TYPES: BEGIN OF ty_agrupa,
         vbeln  TYPE vbeln,
         auart  TYPE auart,
         data   TYPE sy-datum,
         inco   TYPE inco1,
         werks  TYPE werks,
         vbelns TYPE string,
       END OF ty_agrupa,

       BEGIN OF ty_ibupacustomer,
         customer        TYPE ibupacustomer-customer,
         businesspartner TYPE ibupacustomer-businesspartner,
       END OF ty_ibupacustomer,

       BEGIN OF ty_but050,
         partner1 TYPE but050-partner1,
         partner2 TYPE but050-partner2,
       END OF ty_but050,

       BEGIN OF ty_but000,
         partner   TYPE but000-partner,
         name_last TYPE but000-name_last,
         marst     TYPE but000-marst,
       END OF ty_but000,

       BEGIN OF ty_but020,
         partner    TYPE but020-partner,
         addrnumber TYPE but020-addrnumber,
       END OF ty_but020,

       BEGIN OF ty_adrc,
         addrnumber TYPE adrc-addrnumber,
         city1      TYPE adrc-city1,
         city2      TYPE adrc-city2,
         street     TYPE adrc-street,
         house_num1 TYPE adrc-house_num1,
         region     TYPE adrc-region,
       END OF ty_adrc,

       BEGIN OF ty_dfkkbptaxnum,
         partner TYPE dfkkbptaxnum-partner,
         taxtype TYPE dfkkbptaxnum-taxtype,
         taxnum  TYPE dfkkbptaxnum-taxnum,
       END OF ty_dfkkbptaxnum.



DATA: it_agrupa TYPE TABLE OF zsds016.
DATA: it_parceiros TYPE TABLE OF zsds017 WITH HEADER LINE.
DATA: it_aux TYPE TABLE OF zsds016 WITH HEADER LINE.

DATA:
  it_contrato       TYPE TABLE OF zsdt0040,
  it_itens          TYPE TABLE OF zsdt0041,
  it_itens_con      TYPE TABLE OF zsdt0041,
  it_itens_aux      TYPE TABLE OF zsdt0041,
  it_kna1           TYPE TABLE OF kna1,
  it_kna1_i         TYPE TABLE OF kna1,
  it_kna1_c         TYPE TABLE OF kna1,
  it_kna1_f         TYPE TABLE OF kna1,
  it_makt           TYPE TABLE OF makt,
  it_mara           TYPE TABLE OF mara,
  it_023            TYPE TABLE OF t023t,
  it_itens_im       TYPE TABLE OF zcontrato_insumos_itens,
  it_ibupacustomer  TYPE TABLE OF ty_ibupacustomer,
  it_but050         TYPE TABLE OF ty_but050,
  it_dfkkbptaxnum   TYPE TABLE OF ty_dfkkbptaxnum,
  it_but000         TYPE TABLE OF ty_but000,
  it_but000_cliente TYPE TABLE OF ty_but000,
  it_but020         TYPE TABLE OF ty_but020,
  it_adrc           TYPE TABLE OF ty_adrc,
  it_41             TYPE TABLE OF ty_0041,
  it_zsdt0090       TYPE TABLE OF zsdt0090,

  wa_contrato       TYPE zsdt0040,
  wa_itens          TYPE zsdt0041,
  wa_kna1           TYPE kna1,
  wa_kna1_i         TYPE kna1,
  wa_kna1_c         TYPE kna1,
  wa_kna1_f         TYPE kna1,
  wa_makt           TYPE makt,
  wa_mara           TYPE mara,
  wa_023            TYPE t023t,
  wa_import         TYPE zcontrato_insumos,
  wa_itens_im       TYPE zcontrato_insumos_itens,
  wa_41             TYPE ty_0041,
  wa_ibupacustomer  TYPE ty_ibupacustomer,
  wa_but050         TYPE ty_but050,
  wa_but020         TYPE ty_but020,
  wa_adrc           TYPE ty_adrc,
  wa_dfkkbptaxnum   TYPE ty_dfkkbptaxnum,
  wa_but000         TYPE ty_but000,
  wa_but000_cliente TYPE ty_but000,
  r_vbeln           TYPE RANGE OF vbeln,
  s_doc_simulacao   TYPE RANGE OF zsded003,  "*-CS2019001753-27.02.2023-#65723-JT

  convert           TYPE n LENGTH 10,
  c_vbeln           TYPE c LENGTH 255,
  taxa              TYPE ukurs_curr,
  data              TYPE sy-datum,
  valor(8)          TYPE p DECIMALS 2.

DATA:
  vg_name     LIKE thead-tdname,
  vg_language LIKE thead-tdspras,
  vg_object   LIKE thead-tdobject,
  vg_line     TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE,
  it_matnr    TYPE TABLE OF tline INITIAL SIZE 0 WITH HEADER LINE.

* Estrutura para Palavras por extenso
DATA:
  BEGIN OF palavra,
    inteiro     LIKE spell-word,
    real(6),
    dolar(255),
    filler(3),
    decimal     LIKE spell-decword,
    centavos(8),
  END OF palavra.

DATA:
  decimals TYPE p,
  word     LIKE spell.

RANGES: rg_taxtype FOR dfkkbptaxnum-taxtype.



*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_DOC      Numero da Doc de Simulação
*----------------------------------------------------------------------*
FORM seleciona_dados USING i_doc.

  REFRESH: it_contrato, it_itens, it_kna1, it_makt.

*-CS2019001753-27.02.2023-#65723-JT-inicio
  IF i_doc IS NOT INITIAL.
    SELECT *  FROM zsdt0040
      INTO TABLE it_contrato
      WHERE doc_simulacao EQ i_doc.
  ELSE.
    SELECT *  FROM zsdt0040
      INTO TABLE it_contrato
      WHERE doc_simulacao IN s_doc_simulacao.
  ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

  IF it_contrato IS NOT INITIAL.

    CLEAR rg_taxtype[].
    rg_taxtype-sign   = 'I'.
    rg_taxtype-option = 'EQ'.
    rg_taxtype-low    = 'BR2'.
    APPEND rg_taxtype.
    rg_taxtype-low    = 'BR4'.
    APPEND rg_taxtype.


    " Início - Dados sócios - TRANSPORTE BOLETO ITAU PSA - DEVK9A1R58 - 16/11/2023
    SELECT customer businesspartner
           FROM ibupacustomer
           INTO TABLE it_ibupacustomer
           FOR ALL ENTRIES IN it_contrato
           WHERE customer EQ it_contrato-kunnr.

    IF NOT it_ibupacustomer[] IS INITIAL.
      SORT it_ibupacustomer BY customer businesspartner.

      SELECT partner name_last marst
             FROM but000
             INTO TABLE it_but000_cliente
             FOR ALL ENTRIES IN it_ibupacustomer
             WHERE partner EQ it_ibupacustomer-businesspartner.


      SELECT partner1 partner2 UP TO 1 ROWS
             FROM but050
             INTO TABLE it_but050
             FOR ALL ENTRIES IN it_ibupacustomer
             WHERE partner1 EQ it_ibupacustomer-businesspartner
             AND   reltyp   EQ 'CRME01'.

    ENDIF.

    IF NOT it_but050[] IS INITIAL.
      SORT it_but050 BY partner1 partner2.

      SELECT partner name_last marst
             FROM but000
             INTO TABLE it_but000
             FOR ALL ENTRIES IN it_but050
             WHERE partner EQ it_but050-partner2.

      SELECT partner taxtype taxnum
          FROM dfkkbptaxnum
          INTO TABLE it_dfkkbptaxnum
          FOR ALL ENTRIES IN it_but050
          WHERE partner EQ it_but050-partner2
          AND   taxtype IN rg_taxtype.

    ENDIF.

    IF NOT it_but000[] IS INITIAL.
      SORT it_but000 BY partner.

      SELECT partner addrnumber
             FROM but020
             INTO TABLE it_but020
             FOR ALL ENTRIES IN it_but000
             WHERE partner EQ it_but000-partner.
    ENDIF.

    IF NOT it_but020[] IS INITIAL.
      SORT it_but020 BY partner.

      SELECT addrnumber city1 city2 street house_num1 region
             FROM adrc
             INTO TABLE it_adrc
             FOR ALL ENTRIES IN it_but020
             WHERE addrnumber EQ it_but020-addrnumber.
    ENDIF.

    SORT it_adrc BY addrnumber.

    LOOP AT it_contrato INTO wa_contrato.

      READ TABLE it_ibupacustomer INTO wa_ibupacustomer WITH KEY customer = wa_contrato-kunnr BINARY SEARCH.
      IF sy-subrc EQ 0.

        READ TABLE it_but050 INTO wa_but050 WITH KEY partner1 = wa_ibupacustomer-businesspartner BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE it_but000 INTO wa_but000 WITH KEY partner = wa_but050-partner2 BINARY SEARCH.
          IF sy-subrc EQ 0.
            SPLIT wa_but000-name_last AT ' ' INTO it_parceiros-namev it_parceiros-name1.
            it_parceiros-ftext    = wa_but000-marst.
            it_parceiros-partner2 = wa_but050-partner2.

            READ TABLE it_but020 INTO wa_but020 WITH KEY partner = wa_but000-partner BINARY SEARCH.
            IF sy-subrc EQ 0.
              READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber = wa_but020-addrnumber BINARY SEARCH.
              IF sy-subrc EQ 0.
                it_parceiros-city1      = wa_adrc-city1.
                it_parceiros-city2      = wa_adrc-city2.
                it_parceiros-street     = wa_adrc-street.
                it_parceiros-house_num1 = wa_adrc-house_num1.
                it_parceiros-region     = wa_adrc-region.

                READ TABLE it_dfkkbptaxnum INTO wa_dfkkbptaxnum WITH KEY partner = wa_but050-partner2
                                                                         taxtype = 'BR4'.
                IF sy-subrc EQ 0.
                  it_parceiros-rg = wa_dfkkbptaxnum-taxnum.
                ENDIF.

                READ TABLE it_dfkkbptaxnum INTO wa_dfkkbptaxnum WITH KEY partner = wa_but050-partner2
                                                                         taxtype = 'BR2'.
                IF sy-subrc EQ 0.
                  it_parceiros-cpf = wa_dfkkbptaxnum-taxnum.
                ENDIF.

                APPEND it_parceiros.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

*      it_parceiros-rg_exp
*      it_parceiros-BEZEI

    ENDLOOP.
    " Fim - Dados sócios - TRANSPORTE BOLETO ITAU PSA - DEVK9A1R58 - 16/11/2023

    IF it_parceiros[] IS INITIAL.

      SELECT a~namev a~name1 d~civil e~tel_number e~tel_extens b~fax_number
       c~city1 c~city2 c~street c~house_num1 c~region
       FROM knvk AS a
       LEFT JOIN adr3 AS b ON b~persnumber EQ a~prsnr
       LEFT JOIN adrc AS c ON c~addrnumber EQ a~adrnp_2
       LEFT JOIN t7bre1 AS d ON d~famst EQ a~famst AND
                                d~sprsl EQ sy-langu
       LEFT JOIN adr2 AS e ON e~persnumber EQ a~prsnr
       INTO TABLE it_parceiros
       FOR ALL ENTRIES IN it_contrato
       WHERE kunnr EQ it_contrato-kunnr
       AND pafkt EQ '12'.

    ENDIF.


    IF it_parceiros[] IS NOT INITIAL.

      SELECT * FROM t005u
         INTO TABLE @DATA(it_estados)
         FOR ALL ENTRIES IN @it_parceiros
         WHERE bland EQ @it_parceiros-region
           AND land1 EQ 'BR'
           AND spras EQ @sy-langu.

      LOOP AT it_parceiros ASSIGNING FIELD-SYMBOL(<parceiros>).

        <parceiros>-ftext = |{ <parceiros>-ftext CASE = LOWER }|.

        WRITE <parceiros>-cpf USING EDIT MASK   '___.___.___-__'  TO <parceiros>-cpf.

        PERFORM upper_lower USING 'R' <parceiros>-city1  CHANGING <parceiros>-city1.
        PERFORM upper_lower USING 'R' <parceiros>-city2  CHANGING <parceiros>-city2.
        PERFORM upper_lower USING 'R' <parceiros>-street CHANGING <parceiros>-street.

        TRY .
            <parceiros>-bezei = it_estados[ bland = <parceiros>-region ]-bezei.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDLOOP.

    ENDIF.

    SELECT * FROM zsdt0041
      INTO TABLE it_itens
      FOR ALL ENTRIES IN it_contrato
      WHERE doc_simulacao EQ it_contrato-doc_simulacao.

    IF it_itens IS NOT INITIAL.

      r_vbeln = VALUE #( FOR ls IN it_itens LET s = 'I' o = 'EQ' IN sign = s option = o ( low = ls-vbeln ) ).

      SELECT MAX( erdat ) FROM vbak
        INTO data
        WHERE vbeln IN r_vbeln.

    ENDIF.

    SELECT * FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_contrato
      WHERE kunnr EQ it_contrato-kunnr.

    SELECT doc_simulacao werks
      FROM zsdt0041
      INTO TABLE it_41
      FOR ALL ENTRIES IN it_itens
      WHERE doc_simulacao EQ it_itens-doc_simulacao.

    SELECT *
      FROM zsdt0090
      INTO TABLE it_zsdt0090
      FOR ALL ENTRIES IN it_itens
      WHERE vbelv EQ it_itens-vbeln
      AND categoria EQ 'O'.

  ENDIF.

  LOOP AT it_41 INTO wa_41.
    WRITE wa_41-werks USING EDIT MASK '000000____' TO wa_41-werks_fk.
    MODIFY it_41 INDEX sy-tabix FROM wa_41 TRANSPORTING werks_fk.
  ENDLOOP.

  IF it_41 IS NOT INITIAL.
    SELECT * FROM kna1
       INTO TABLE it_kna1_i
       FOR ALL ENTRIES IN it_41
       WHERE kunnr EQ it_41-werks_fk.
  ENDIF.

  IF it_itens IS NOT INITIAL.
    SELECT * FROM mara
      INTO TABLE it_mara
      FOR ALL ENTRIES IN it_itens
      WHERE matnr EQ it_itens-matnr.

    IF it_mara IS NOT INITIAL.
      SELECT * FROM t023t
        INTO TABLE it_023
        FOR ALL ENTRIES IN it_mara
        WHERE matkl EQ it_mara-matkl
        AND spras EQ sy-langu.
    ENDIF.

    SELECT * FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_itens
      WHERE matnr EQ it_itens-matnr
        AND spras EQ sy-langu.

  ENDIF.
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VBELN
*&---------------------------------------------------------------------*
*       Busca todos os Vbeln dos Itens
*----------------------------------------------------------------------*
FORM busca_vbeln.

  CLEAR: c_vbeln.
  MOVE it_itens TO it_itens_aux.
  SORT it_itens_aux BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_itens_aux COMPARING vbeln.

  LOOP AT it_itens_aux INTO wa_itens.
    SHIFT wa_itens-vbeln LEFT DELETING LEADING '0'.
    CONCATENATE c_vbeln wa_itens-vbeln INTO c_vbeln SEPARATED BY ', '.
  ENDLOOP.

  SHIFT c_vbeln LEFT DELETING LEADING ','.

ENDFORM.                    " BUSCA_VBELN

*&---------------------------------------------------------------------*
*&      Form  AGRUPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM agrupa_dados USING p_dir
                        p_no_print ""*-CS2019001753-27.02.2023-#65723-Jt
               CHANGING p_xstring. "*-CS2019001753-27.02.2023-#65723-Jt

  DATA: wa_saida TYPE zcontrato_insumos.
  DATA: p_string TYPE string.

  CLEAR: wa_saida, wa_import.

  LOOP AT it_contrato INTO wa_contrato.

    wa_saida-v_vbeln_e = wa_contrato-doc_simulacao.
    wa_saida-v_vbeln_i = c_vbeln.
    wa_saida-cultura   = wa_contrato-cultura.
    wa_saida-safra   = wa_contrato-safra.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_contrato-kunnr.
    wa_saida-id_nome  = wa_contrato-kunnr.  " ID Cliente
    wa_saida-nome     = wa_kna1-name1 && wa_kna1-name2.  " NOME Cliente
    wa_saida-uf       = wa_kna1-regio.      " UF Cliente
    wa_saida-stkzn    = wa_kna1-stkzn.

    IF wa_kna1-stkzn IS NOT INITIAL.
      MOVE wa_kna1-stcd4 TO wa_saida-rg.   " RG  Cliente
*      PERFORM CONVERTRG CHANGING WA_SAIDA-RG.

      WRITE wa_kna1-stcd2 USING EDIT MASK '___.___.___-__'      TO wa_saida-cpf.  " CPF Cliente
    ELSE.
      WRITE wa_kna1-stcd1 USING EDIT MASK '__.___.___/____-__'  TO wa_saida-cnpj. " CNPJ Cliente
    ENDIF.

    PERFORM upper_lower USING 'R' wa_kna1-stras CHANGING wa_saida-rua.    " Rua Cliente
    PERFORM upper_lower USING 'C' wa_kna1-mcod3 CHANGING wa_saida-cidade. " Cidade Cliente
    PERFORM upper_lower USING 'E' wa_kna1-regio CHANGING wa_saida-estado. " Estado Cliente

    PERFORM upper_lower USING 'R' wa_kna1-ort02 CHANGING wa_saida-local.    " Local

    " Início - Dados Conjuge - TRANSPORTE BOLETO ITAU PSA - DEVK9A1R58 - 16/11/2023
    CLEAR: wa_but050, wa_ibupacustomer, wa_but000, it_dfkkbptaxnum[].
    SELECT customer businesspartner UP TO 1 ROWS
          FROM ibupacustomer
          INTO wa_ibupacustomer
          WHERE customer EQ wa_contrato-kunnr.
    ENDSELECT.

    IF NOT wa_ibupacustomer-businesspartner IS INITIAL.

      READ TABLE it_but000_cliente INTO wa_but000_cliente WITH KEY partner = wa_ibupacustomer-businesspartner.
      IF NOT wa_but000_cliente IS INITIAL. "estado civil
        SELECT SINGLE bez20
                FROM tb027t
                INTO wa_saida-situacao
                WHERE marst EQ wa_but000_cliente-marst
                AND spras EQ sy-langu.
      ELSE.
        SELECT SINGLE vtext
                FROM tvk1t
                INTO wa_saida-situacao
                WHERE katr1 EQ wa_kna1-katr1
                AND spras EQ sy-langu.
      ENDIF.
      wa_saida-situacao = |{ wa_saida-situacao CASE = LOWER }|.

      SELECT partner1 partner2 UP TO 1 ROWS
             FROM but050
             INTO wa_but050
             WHERE partner1 EQ wa_ibupacustomer-businesspartner
             AND   reltyp   EQ 'BUR004'.
      ENDSELECT.

      IF NOT wa_but050 IS INITIAL.

        SELECT SINGLE partner name_last marst
               FROM but000
               INTO wa_but000
               WHERE partner EQ wa_but050-partner2.

        SELECT partner taxtype taxnum
               FROM dfkkbptaxnum
               INTO TABLE it_dfkkbptaxnum
               WHERE partner EQ wa_but050-partner2
               AND   taxtype IN rg_taxtype.
      ENDIF.


    ENDIF.
    " Fim - Dados Conjuge - TRANSPORTE BOLETO ITAU PSA - DEVK9A1R58 - 16/11/2023

    IF wa_but050 IS INITIAL.

      SELECT *
        FROM knvk
        INTO TABLE @DATA(it_knvk)
        WHERE kunnr EQ @wa_contrato-kunnr
         AND pafkt EQ '11'.

      IF  it_knvk IS NOT INITIAL.

        wa_saida-s_c = 'C'.

        SELECT *
          FROM adr3
           INTO TABLE @DATA(it_adr3)
            FOR ALL ENTRIES IN @it_knvk
            WHERE persnumber EQ @it_knvk-prsnr.

        SELECT *
          FROM adr2
           INTO TABLE @DATA(it_adr2)
            FOR ALL ENTRIES IN @it_knvk
            WHERE persnumber EQ @it_knvk-prsnr.

        TRY .
            DATA(wa_knvk) = it_knvk[ kunnr = wa_contrato-kunnr ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR wa_knvk.
        ENDTRY.

        TRY.
            DATA(wa_adr3) = it_adr3[ persnumber = wa_knvk-prsnr ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR wa_adr3.
        ENDTRY.

        TRY.
            DATA(wa_adr2) = it_adr2[ persnumber = wa_knvk-prsnr ].
          CATCH cx_sy_itab_line_not_found.
            CLEAR wa_adr2.
        ENDTRY.


        wa_saida-id_nome_c  = wa_knvk-parnr.                          " Id Conjuge Cliente
        wa_saida-nome_c     = |{ wa_knvk-namev } { wa_knvk-name1 }|.  " Nome Conjuge Cliente

        TRANSLATE wa_saida-nome_c TO UPPER CASE.
        wa_saida-rg_c = |{ wa_adr2-tel_number }-{ wa_adr2-tel_extens }|.
*      WRITE WA_KNVK-TELF1      USING EDIT MASK '_______-_'       TO WA_SAIDA-RG_C.  " Rg Conjuge Cliente
        WRITE wa_adr3-fax_number USING EDIT MASK '___.___.___-__'  TO wa_saida-cpf_c. " CPF conjuge Cliente

      ENDIF.

    ELSE.

      wa_saida-s_c = 'C'.

      wa_saida-id_nome_c  = wa_but050-partner2.                          " Id Conjuge Cliente
      wa_saida-nome_c     = wa_but000-name_last.                         " Nome Conjuge Cliente
      TRANSLATE wa_saida-nome_c TO UPPER CASE.

      READ TABLE it_dfkkbptaxnum INTO wa_dfkkbptaxnum WITH KEY partner = wa_but050-partner2
                                                               taxtype = 'BR4'.
      IF sy-subrc EQ 0.
        wa_saida-rg_c = wa_dfkkbptaxnum-taxnum.
      ENDIF.

      READ TABLE it_dfkkbptaxnum INTO wa_dfkkbptaxnum WITH KEY partner = wa_but050-partner2
                                                               taxtype = 'BR2'.
      IF sy-subrc EQ 0.
*      WRITE WA_KNVK-TELF1      USING EDIT MASK '_______-_'       TO WA_SAIDA-RG_C.  " Rg Conjuge Cliente
        WRITE wa_dfkkbptaxnum-taxnum USING EDIT MASK '___.___.___-__'  TO wa_saida-cpf_c. " CPF conjuge Cliente
      ENDIF.


    ENDIF.

    wa_saida-waerk        = wa_contrato-waerk. " Moeda
    PERFORM busca_taxa USING data CHANGING taxa.

    wa_saida-tpsim = wa_contrato-tpsim.

    IF wa_contrato-waerk EQ 'USD'.
      wa_saida-reais = wa_contrato-vlrtot * taxa. " Valor em Reais * taxa
    ELSE.
      wa_saida-reais = wa_contrato-vlrtot.        " Valor em Reais
    ENDIF.


    wa_saida-venc = COND #( WHEN NOT wa_contrato-dtvencov IS INITIAL
                                            THEN wa_contrato-dtvencov
                                            ELSE wa_contrato-dtpgtcult ).

    wa_saida-dolar      = wa_contrato-vlrtot.         " Valor em Dolares
    wa_saida-juros      = wa_contrato-juros_mora.     " Juros
    wa_saida-venc_dia   = wa_saida-venc+6(2). " Vencimento DIA
    wa_saida-venc_mes   = wa_saida-venc+4(2). " Vencimento MES
    wa_saida-venc_ano   = wa_saida-venc(4).   " Vencimento ANO
    BREAK wbarbosa.
    PERFORM valor_extenso USING 'BRL' wa_saida-reais CHANGING wa_saida-reais_desc. " Processa o Nome por Extenso em Reais.
    PERFORM valor_extenso USING 'USD' wa_saida-dolar CHANGING wa_saida-dolar_desc. " Processa o Nome por Extenso em Dolares.
    PERFORM valor_extenso USING '   ' wa_saida-juros CHANGING wa_saida-juros_desc. " Processa o Nome por Extenso do Juros.

    wa_saida-pag_prorrogado = wa_contrato-pag_prorrogado. " Verifica se é Prorrogado SIM(X)  NÂO()
    wa_saida-juros_dia      = wa_contrato-dtpgtcult+6(2). " Dia juros
    wa_saida-juros_dia      = wa_saida-juros_dia + 1.     " Dia juros + 1
    wa_saida-juros_mes      = wa_contrato-dtpgtcult+4(2). " Mes juros
    wa_saida-juros_ano      = wa_contrato-dtpgtcult(4).   " Ano juros

*#########################  FIADOR 01 ################################## INICIO

    SELECT * FROM kna1 INTO TABLE it_kna1_f
      WHERE kunnr EQ wa_contrato-fiador_01.

    READ TABLE it_kna1_f INTO wa_kna1_f WITH KEY kunnr = wa_contrato-fiador_01.

    wa_saida-f1           = wa_contrato-fiador_01.
    wa_saida-f1_name      = wa_kna1_f-name1 && wa_kna1_f-name2.
    wa_saida-prof_f1      = '##PROFISSÃO##'.
    wa_saida-rg_f1        = wa_kna1_f-stcd3.
    wa_saida-orgao_rg_f1  = '##SSP##'.
    wa_saida-cpf_f1       = wa_kna1_f-stcd2.

    IF wa_kna1_f-konzs IS NOT INITIAL.
      wa_saida-s_f1 = 'C'.
      convert = wa_kna1_f-konzs.

      SELECT * FROM kna1 INTO TABLE it_kna1_f
      WHERE kunnr EQ convert.

      wa_saida-id_f1_c        = wa_kna1_f-kunnr.
      wa_saida-f1_c           = wa_kna1_f-name1 && wa_kna1_f-name2.
      wa_saida-prof_f1_c      = '##PROFISSÃO##'.
      wa_saida-rg_f1_c        = wa_kna1_f-stcd3.
      wa_saida-orgao_rg_f1_c  = '##SSP##'.
      wa_saida-cpf_f1_c       = wa_kna1_f-stcd2.

    ENDIF.

    wa_saida-end_f1           = wa_kna1_f-stras.
    wa_saida-cidade_f1        = wa_kna1_f-mcod3.
    wa_saida-uf_f1            = wa_kna1_f-regio.
*#########################  FIADOR 01 ################################## FIM


*#########################  FIADOR 02 ################################## INICIO

    REFRESH it_kna1_f.
    CLEAR wa_kna1_f.

    SELECT * FROM kna1 INTO TABLE it_kna1_f
      WHERE kunnr EQ wa_contrato-fiador_02.

    READ TABLE it_kna1_f INTO wa_kna1_f WITH KEY kunnr = wa_contrato-fiador_02.

    wa_saida-f2               = wa_contrato-fiador_02.
    wa_saida-f2_name          = wa_kna1_f-name1 && wa_kna1_f-name2.
    wa_saida-prof_f2          = '##PROFISSÃO##'.
    wa_saida-rg_f2            = wa_kna1_f-stcd3.
    wa_saida-orgao_rg_f2      = '##SSP##'.
    wa_saida-cpf_f2           = wa_kna1_f-stcd2.

    IF wa_kna1_f-konzs IS NOT INITIAL.

      wa_saida-s_f2 = 'C'.
      convert = wa_kna1_f-konzs.

      SELECT * FROM kna1 INTO TABLE it_kna1_f
      WHERE kunnr EQ convert.

      wa_saida-id_f2_c        = wa_kna1_f-kunnr.
      wa_saida-f2_c           = wa_kna1_f-name1 && wa_kna1_f-name2.
      wa_saida-prof_f2_c      = '##PROFISSÃO##'.
      wa_saida-rg_f2_c        = wa_kna1_f-stcd3.
      wa_saida-orgao_rg_f2_c  = '##SSP##'.
      wa_saida-cpf_f2_c       = wa_kna1_f-stcd2.

    ENDIF.

    wa_saida-end_f2           = wa_kna1_f-stras.
    wa_saida-cidade_f2        = wa_kna1_f-mcod3.
    wa_saida-uf_f2            = wa_kna1_f-regio.
*#########################  FIADOR 02 ##################################  FIM


    wa_saida-dia = data+6(2).
    wa_saida-mes = data+4(2).
    wa_saida-ano = data(4).

    wa_saida-dt_entrega_sem = wa_contrato-dt_entrega_sem.
    wa_saida-dt_entrega_def = wa_contrato-dt_entrega_def.
    wa_saida-dt_entrega_fet = wa_contrato-dt_entrega_fet.

    MOVE-CORRESPONDING wa_saida TO wa_import.

  ENDLOOP.

  PERFORM import_itens.

  PERFORM agruda_item_iv.

  DATA: vl_formname         TYPE tdsfname,
        vl_name             TYPE rs38l_fnam,
        ls_options          TYPE ssfcompop,
        ls_control          TYPE ssfctrlop,    "*-CS2019001753-27.02.2023-#65723-JT
        ls_job_output_info  TYPE ssfcrescl,    "*-CS2019001753-27.02.2023-#65723-JT
        ls_otfdata          TYPE tsfotf,       "*-CS2019001753-27.02.2023-#65723-JT
        ls_bin_fsize        TYPE i,            "*-CS2019001753-27.02.2023-#65723-JT
        ls_xstring_document TYPE xstring,      "*-CS2019001753-27.02.2023-#65723-JT
        t_lines             TYPE STANDARD TABLE OF tline, "*-CS2019001753-27.02.2023-#65723-JT
        control             TYPE ssfctrlop.

*  VL_FORMNAME = 'Z_TESTE_44'.
  vl_formname = 'ZSDF0006'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

*  Impresora
  ls_options-tddest   = 'LOCL'.     "Disposit. saída
  ls_options-tdimmed  = abap_true.  "Saída Imediata
  ls_options-tdnewid  = abap_true.  "Nova Ordem SPOOL
  ls_options-tdcovtitle = |Contrato Simulador nº { wa_contrato-doc_simulacao }_{ sy-uname }_{ sy-datum }_{ sy-uzeit }|. "Titulo

*-CS2019001753-27.02.2023-#65723-JT-inicio
  IF p_no_print = abap_true.
    ls_control-no_dialog = abap_true.
    ls_control-langu     = sy-langu.
    ls_control-getotf    = abap_true.
  ELSE.
    CLEAR ls_control.
  ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      output_options     = ls_options
      control_parameters = ls_control  "*-CS2019001753-27.02.2023-#65723-JT
      wa_import          = wa_import
    IMPORTING
      job_output_info    = ls_job_output_info "*-CS2019001753-27.02.2023-#65723-JT
    TABLES
      it_itens_im        = it_itens_im[]
      it_iteniv          = it_aux[]
      it_parceiros       = it_parceiros[]
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Formulário não será exibido.' DISPLAY LIKE 'E'.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-CS2019001753-27.02.2023-#65723-JT-inicio
  IF p_no_print = abap_true.
    MOVE ls_job_output_info-otfdata[] TO ls_otfdata[].

*-------------------------------------------
*-format xstring
*-------------------------------------------
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_filesize          = ls_bin_fsize
        bin_file              = ls_xstring_document
      TABLES
        otf                   = ls_otfdata[]
        lines                 = t_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

    IF sy-subrc = 0.
      p_xstring = ls_xstring_document.
    ENDIF.
  ENDIF.
*-CS2019001753-27.02.2023-#65723-JT-fim

ENDFORM.                    " AGRUPA_DADOS


*&---------------------------------------------------------------------*
*&      Form  BUSCA_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_DATA     data de entrada da Taxa
*      -->P_TAXA     taxa do Cabio do Dia ou o anterior
*----------------------------------------------------------------------*
FORM busca_taxa  USING    i_data
                 CHANGING p_taxa.

  DATA(obj_zcl_util_sd) = NEW zcl_util_sd( ).

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').

  MOVE  i_data TO vl_gdatu.
  obj_zcl_util_sd->set_data( vl_gdatu ).
  p_taxa = obj_zcl_util_sd->taxa_cambio( ).

ENDFORM.                    " BUSCA_TAXA
*&---------------------------------------------------------------------*
*&      Form  UPPER_LOWER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VAR      'R->Rua, C->Cidade, E->Estado'
*      -->P_IMPUT    valor para conversão
*      -->P_EXPORT   valor convertido com os digitos minusculo
*----------------------------------------------------------------------*
FORM upper_lower  USING    p_var
                           p_imput
                  CHANGING p_export.

  TYPES: BEGIN OF ty_string,
           string(255),
         END OF ty_string.

  DATA: wa_string  TYPE ty_string,
        it_string  TYPE STANDARD TABLE OF ty_string,

        wa_estados TYPE t005u,
        it_estados TYPE TABLE OF t005u.

  SELECT * FROM t005u
    INTO TABLE it_estados
    WHERE land1 EQ 'BR'
    AND spras EQ 'P'.

  DELETE it_estados WHERE bezei IS INITIAL.

  TRANSLATE p_imput TO UPPER CASE.
  SPLIT p_imput AT ' ' INTO TABLE it_string.
  CLEAR p_imput.

  LOOP AT it_string INTO wa_string.

    CASE p_var.
      WHEN 'R' OR 'C'.

        READ TABLE it_estados TRANSPORTING NO FIELDS WITH KEY bland = wa_string-string.
        IF sy-subrc IS NOT INITIAL.

          TRANSLATE wa_string-string+1 TO LOWER CASE.
          CONCATENATE p_imput wa_string-string INTO p_imput SEPARATED BY ' '.
        ELSE.

          TRANSLATE wa_string-string TO UPPER CASE.
          CONCATENATE p_imput wa_string-string INTO p_imput SEPARATED BY ' '.
        ENDIF.

      WHEN 'E'.

        READ TABLE it_estados INTO wa_estados WITH KEY bland = wa_string-string.
        p_export = wa_estados-bezei.
    ENDCASE.

  ENDLOOP.

  IF p_var EQ 'R' OR p_var EQ 'C'.
    p_imput = p_imput+1.
    MOVE p_imput TO p_export.
  ENDIF.

ENDFORM.                    " UPPER_LOWER

*&---------------------------------------------------------------------*
*&      Form  VALOR_EXTENSO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TIPO       Moeda 'BRL ou USD'
*      -->I_VALOR    Valor para conversão
*      -->E_VALOR    Valor convertido por extenso
*----------------------------------------------------------------------*
FORM valor_extenso USING tipo i_valor CHANGING e_valor.
  valor = i_valor.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount    = valor
      currency  = 'BRL'
      filler    = ' '
      language  = sy-langu
    IMPORTING
      in_words  = word
    EXCEPTIONS
      not_found = 1
      too_large = 2
      OTHERS    = 3.

  palavra-inteiro = word-word.

  IF tipo EQ 'BRL'.
    IF valor > 1.
      palavra-real = 'REAIS'.
    ELSE.
      palavra-real = 'REAL'.
    ENDIF.
  ELSEIF tipo EQ 'USD'.
    IF valor > 1.
      palavra-dolar = 'DÓLARES DOS ESTADOS UNIDOS'.
    ELSE.
      palavra-dolar = 'DÓLAR DOS ESTADOS UNIDOS'.
    ENDIF.
  ENDIF.

  IF word-decword NE ' ' AND word-decword NE 'ZERO'.
    IF tipo NE ' '.
      palavra-filler = ' E '.
      palavra-centavos = 'CENTAVOS'.
      palavra-decimal = word-decword.
    ENDIF.
  ENDIF.

  CONDENSE palavra.
  e_valor = palavra.
  TRANSLATE e_valor TO LOWER CASE.

ENDFORM.                    "VALOR_EXTENSO


*&---------------------------------------------------------------------*
*&      Form  IMPORT_ITENS
*&---------------------------------------------------------------------*
*       Montandos a tabela para enviar para o SmartForm
*----------------------------------------------------------------------*
FORM import_itens .

  DATA: p_string TYPE string.
  REFRESH: it_itens_im.

  LOOP AT it_itens INTO wa_itens.

    wa_itens_im-sa    = wa_itens-spart.
    wa_itens_im-vbeln = wa_itens-vbeln.
    wa_itens_im-auar  = wa_itens-auart.


    FREE: vg_line[].
    PERFORM reade_text USING 'GRUN' sy-langu wa_itens-matnr 'MATERIAL'.

    CALL FUNCTION 'ZFORMATA_LINES'
      TABLES
        lines   = vg_line
        lines_s = it_matnr.

    LOOP AT it_matnr.
      wa_itens_im-uni = |{ wa_itens_im-uni } { it_matnr-tdline }|.
    ENDLOOP.

    FREE: it_matnr[], vg_line[].

*    WA_ITENS_IM-UNI   = WA_ITENS-ZIEME.

    WRITE wa_itens-zmeng TO wa_itens_im-qtd DECIMALS 3 LEFT-JUSTIFIED NO-GAP.

*    SHIFT WA_ITENS_IM-QTD LEFT DELETING LEADING '0'.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_itens-matnr.
    wa_itens_im-cod  = wa_makt-maktx.
    wa_itens_im-matnr  = wa_itens-matnr.


    p_string = wa_itens-zwert.
    PERFORM f_format_value(zsdr016) CHANGING p_string.
    wa_itens_im-vun   = p_string.

*    ADD WA_ITENS-VLR_ICMS TO WA_ITENS-VLRTOT.

* "// Iten comentado, item da OV sofreu alteração impactando no total do simulador. 11.10.18
*    PERFORM ADD_IMPOSTO CHANGING WA_ITENS.

* "// descomentando a rotina de pegar os desconto da OV para adicionar no Item na 41. 11.10.18
    LOOP AT it_zsdt0090 INTO DATA(wa_0090)
      WHERE vbelv   EQ wa_itens-vbeln
      AND matnrv    EQ wa_itens-matnr
      AND categoria EQ 'O'
      AND estorno   EQ abap_false.
      ADD wa_0090-desc_absoluto TO wa_itens-vlrtot.
    ENDLOOP.

    p_string = wa_itens-vlrtot.
    PERFORM f_format_value(zsdr016) CHANGING p_string.
    wa_itens_im-vto   = p_string.

    wa_itens_im-inco1 = wa_itens-inco1.

    CASE wa_itens_im-sa.
      WHEN '03'.
        wa_itens_im-dte = wa_import-dt_entrega_def.
      WHEN '02'.
        wa_itens_im-dte = wa_import-dt_entrega_fet.
      WHEN '04'.
        wa_itens_im-dte = wa_import-dt_entrega_sem.
    ENDCASE.

    IF wa_itens_im-inco1 EQ 'FOB'.

      READ TABLE it_41   INTO wa_41 WITH KEY doc_simulacao = wa_itens-doc_simulacao
                                                     werks = wa_itens-werks.
      READ TABLE it_kna1_i INTO wa_kna1_i WITH KEY kunnr = wa_41-werks_fk.

      wa_itens_im-local1 = wa_kna1_i-ort02.
      wa_itens_im-rua    = wa_kna1_i-stras.
      wa_itens_im-cidade = wa_kna1_i-mcod3.
      wa_itens_im-uf     = wa_kna1_i-regio.

    ELSE.
      wa_itens_im-local1 = wa_import-local.
      wa_itens_im-rua    = wa_import-rua.
      wa_itens_im-cidade = wa_import-cidade.
      wa_itens_im-uf     = wa_import-uf.
    ENDIF.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_itens-matnr.
    READ TABLE it_023  INTO wa_023 WITH KEY matkl = wa_mara-matkl
                                            spras = sy-langu.

    wa_itens_im-grupo  = wa_023-wgbez.

    APPEND wa_itens_im TO it_itens_im.
    CLEAR: wa_kna1, wa_itens_im.

  ENDLOOP.

  SORT it_itens_im BY vbeln matnr.

ENDFORM.                    " IMPORT_ITENS
*&---------------------------------------------------------------------*
*&      Form  READE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1409   text
*      -->P_SY_LANGU  text
*      -->P_WA_ITENS_MATNR  text
*      -->P_1412   text
*----------------------------------------------------------------------*
FORM reade_text  USING    p_id
                          p_language
                          p_name
                          p_object.

  vg_language = p_language.
  vg_name     = p_name.
  vg_object   = p_object.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = p_id
      language                = vg_language
      name                    = vg_name
      object                  = vg_object
    TABLES
      lines                   = vg_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGRUDA_ITEM_IV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM agruda_item_iv .

  FREE:  it_agrupa, it_aux.
  CLEAR: it_agrupa, it_aux.

  it_agrupa = VALUE #( FOR ls IN it_itens (
                        seq  = SWITCH #( ls-spart
                                           WHEN '04' THEN 1
                                           WHEN '02' THEN 2
                                           WHEN '03' THEN 3
                                       )
                       vbeln = ls-vbeln
                       spart = ls-spart
                       data  = SWITCH #( ls-spart
                                           WHEN '03' THEN wa_import-dt_entrega_def
                                           WHEN '02' THEN wa_import-dt_entrega_fet
                                           WHEN '04' THEN wa_import-dt_entrega_sem
                                       )
                       inco  = ls-inco1
                       werks = ls-werks
                       name1 = wa_import-local
                     ) ).

  SORT it_agrupa.
  DELETE ADJACENT DUPLICATES FROM it_agrupa COMPARING ALL FIELDS.

  CASE wa_import-cultura.
    WHEN 'SJ'.
      DATA(cultura) = |SOJA|.
    WHEN 'ML'.
      cultura = |MILHO|.
  ENDCASE.

  SORT it_agrupa BY seq.

  LOOP AT it_agrupa INTO DATA(wa_agrupa).

    it_aux-vbeln = wa_agrupa-vbeln.
    it_aux-spart = wa_agrupa-spart.
    it_aux-data  = wa_agrupa-data.
    it_aux-inco  = wa_agrupa-inco.
    it_aux-werks = wa_agrupa-werks.


    IF wa_agrupa-inco NE 'FOB'.
      CLEAR: it_aux-werks, wa_agrupa-werks.
      it_aux-inco = wa_agrupa-inco = 'CIF'.
    ENDIF.

    CASE wa_agrupa-spart.
      WHEN '04'.
        it_aux-texto_sem  = COND #( WHEN it_aux-texto_sem  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ).
      WHEN '02'.
        it_aux-texto_fer  = COND #( WHEN it_aux-texto_fer  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ).
      WHEN '03'.
        it_aux-texto_def  = COND #( WHEN it_aux-texto_def  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ).
    ENDCASE.

    IF NOT line_exists( it_aux[ inco  = wa_agrupa-inco
                                werks = wa_agrupa-werks
                                spart = wa_agrupa-spart ] ).
      APPEND it_aux.
      CLEAR it_aux.
    ELSE.

      DATA(wa_aux) = it_aux[ inco  = wa_agrupa-inco
                             werks = wa_agrupa-werks
                             spart = wa_agrupa-spart ].

      CASE wa_agrupa-spart.
        WHEN '04'.
          wa_aux-texto_sem  = |{ wa_aux-texto_sem  }{ COND #( WHEN wa_aux-texto_sem  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ) }|.
        WHEN '02'.
          wa_aux-texto_fer  = |{ wa_aux-texto_fer  }{ COND #( WHEN wa_aux-texto_fer  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ) }|.
        WHEN '03'.
          wa_aux-texto_def  = |{ wa_aux-texto_def  }{ COND #( WHEN wa_aux-texto_def  IS INITIAL THEN |OV(s) { wa_agrupa-vbeln }| ELSE |, { wa_agrupa-vbeln }| ) }|.
      ENDCASE.

      MOVE-CORRESPONDING wa_aux TO it_aux.

      MODIFY it_aux TRANSPORTING texto_sem
                                 texto_fer
                                 texto_def
                                 alinea
                                 WHERE inco  = wa_agrupa-inco AND
                                       werks = wa_agrupa-werks AND
                                       spart = wa_agrupa-spart AND
                                        data = wa_agrupa-data.
      CLEAR it_aux.
    ENDIF.

  ENDLOOP.

  LOOP AT it_aux ASSIGNING FIELD-SYMBOL(<aux>).

    IF <aux>-texto_sem IS NOT INITIAL.
      <aux>-texto = |{ <aux>-texto  }{ COND #( WHEN <aux>-texto  IS INITIAL THEN |{ <aux>-texto_sem }| ELSE |, { <aux>-texto_sem }| ) }|.
    ENDIF.

    IF <aux>-texto_fer IS NOT INITIAL.
      <aux>-texto = |{ <aux>-texto  }{ COND #( WHEN <aux>-texto  IS INITIAL THEN |{ <aux>-texto_fer }| ELSE |, { <aux>-texto_fer }| ) }|.
    ENDIF.

    IF <aux>-texto_def IS NOT INITIAL.
      <aux>-texto = |{ <aux>-texto  }{ COND #( WHEN <aux>-texto  IS INITIAL THEN |{ <aux>-texto_def }| ELSE |, { <aux>-texto_def }| ) }|.
    ENDIF.

  ENDLOOP.

  LOOP AT it_aux ASSIGNING <aux>.

    IF <aux>-texto_sem IS NOT INITIAL.
      <aux>-alinea = |{ <aux>-alinea }{ COND #( WHEN <aux>-alinea IS INITIAL THEN |"a"| ELSE |, "a"| ) }|.
    ENDIF.

    IF <aux>-texto_fer IS NOT INITIAL.
      <aux>-alinea = |{ <aux>-alinea }{ COND #( WHEN <aux>-alinea IS INITIAL THEN |"b"| ELSE |, "b"| ) }|.
    ENDIF.

    IF <aux>-texto_def IS NOT INITIAL.
      <aux>-alinea = |{ <aux>-alinea }{ COND #( WHEN <aux>-alinea IS INITIAL THEN |"c"| ELSE |, "c"| ) }|.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERTRG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA  text
*----------------------------------------------------------------------*
FORM convertrg  USING p_rg.

  DATA: rg    TYPE c LENGTH 30,
        rg1   TYPE c LENGTH 30,
        orgao TYPE c LENGTH 30,
        BEGIN OF t_valores OCCURS 0,
          valor(5),
        END OF t_valores.

  DATA: cont TYPE i.
  DATA(len) = strlen( p_rg ).
  p_rg = |{ p_rg CASE = UPPER }|.

  DO.
    IF cont >= len.
      EXIT.
    ENDIF.

    IF p_rg+cont(1) CA '0123456789'.
      rg1 = |{ rg1 }{ p_rg+cont(1) }|.
    ELSE.
      orgao = |{ orgao }{ p_rg+cont(1) }|.
    ENDIF.

    ADD 1 TO cont.

  ENDDO.

  CONDENSE orgao NO-GAPS.
  WRITE rg1 USING EDIT MASK '_______-_' TO rg1.
  rg1 = |{ rg1 } { orgao }|.
  p_rg = rg1.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_IMPOSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_imposto CHANGING wa_itens TYPE zsdt0041.

  DATA: coeficiente   TYPE p DECIMALS 5,
        vl_preco_unit TYPE vbap-netpr,
        vl_total      TYPE vbap-netwr,
        vl_quantidade TYPE vbap-kwmeng.

  SELECT SINGLE *
    FROM vbak
    INTO @DATA(wa_vbak)
    WHERE vbeln EQ @wa_itens-vbeln.

  SELECT SINGLE *
    FROM vbap
    INTO @DATA(wa_vbap2)
    WHERE vbeln EQ @wa_itens-vbeln
      AND matnr EQ @wa_itens-matnr.

  SELECT FROM v_konv FIELDS * WHERE kschl IN ( 'PR00' , 'ICVA' , 'ICBS' , 'RB00' ) AND knumv EQ @wa_vbak-knumv INTO TABLE @DATA(it_konv) .


  ADD wa_vbap2-netwr  TO vl_total.       " Valor Total
  ADD wa_vbap2-kwmeng TO vl_quantidade.  " Quantidade

  DATA(wa_konv) = it_konv[ kposn = wa_vbap2-posnr kschl = 'PR00' ].

  IF wa_vbap2-mwsbp IS NOT INITIAL.

    TRY .
        DATA(v_icva) = it_konv[ kposn = wa_vbap2-posnr kschl = 'ICVA' ]-kbetr.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        DATA(v_icbs) = it_konv[ kposn = wa_vbap2-posnr kschl = 'ICBS' ]-kbetr.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY .
        v_icva = v_icva / it_konv[ kposn = wa_vbap2-posnr kschl = 'ICVA' ]-kawrt.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    TRY .
        v_icbs = v_icbs / it_konv[ kposn = wa_vbap2-posnr kschl = 'ICBS' ]-kawrt.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    TRY .
        coeficiente = 1 - ( ( v_icbs * ( v_icva / 100 ) ) / 100 ).
      CATCH cx_sy_zerodivide.
    ENDTRY.

    TRY .
        vl_preco_unit = wa_konv-kbetr / coeficiente.
      CATCH cx_sy_zerodivide.
    ENDTRY.

  ELSE.
    vl_preco_unit  = wa_konv-kbetr.    " Preco unitário
  ENDIF.

  IF wa_vbak-spart EQ '02' OR wa_vbak-spart EQ '04'.
    IF wa_vbap2-vrkme NE wa_konv-kmein.
      CASE wa_konv-kmein.
        WHEN 'TO'.
          vl_quantidade = vl_quantidade / 1000.
        WHEN 'KG'.
          vl_quantidade = vl_quantidade * 1000.
      ENDCASE.
    ENDIF.
  ENDIF.

*  VL_TOTAL  = VL_PRECO_UNIT * VL_QUANTIDADE.
  ADD wa_vbap2-mwsbp TO vl_total.

*  TRY .
*      DATA(DESC_ABS_) = IT_KONV[ KPOSN = WA_VBAP2-POSNR KSCHL = 'RB00' ]-KBETR.
*    CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*  ENDTRY.

*  TRY .
*      DESC_ABS_ = DESC_ABS_ / COEFICIENTE.
*    CATCH CX_SY_ZERODIVIDE.
*  ENDTRY.

*  DATA BASE_LIQ TYPE KBETR.
*
*  TRY .
*      BASE_LIQ =  ( ( DESC_ABS_ / VL_QUANTIDADE ) + WA_KONV-KBETR ).
*    CATCH CX_SY_ZERODIVIDE.
*      CLEAR BASE_LIQ.
*  ENDTRY.
*
*  IF BASE_LIQ NE WA_KONV-KBETR.
*    ADD DESC_ABS_ TO VL_TOTAL.
*  ENDIF.

  wa_itens-vlrtot  =  vl_total.      " Valor Total

ENDFORM.
