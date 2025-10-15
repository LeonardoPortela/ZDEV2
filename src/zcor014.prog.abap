************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 01.11.2013                                          *
* Objetivo    ...: Interface de custos para GEO                        *
* Transação   ...: ZCO0020                                             *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 01.11.2013   Camila Brand          Criação              DEVK933502   *
************************************************************************
REPORT zcor014
    NO STANDARD PAGE HEADING    "Não exibe cabeçalho standard
    LINE-SIZE 076               "Comprimento da Linha
    LINE-COUNT 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
TABLES: coep, coas.

*&--------------------------------------------------------------------&*
*&  ESTRUTURA
*&--------------------------------------------------------------------&*
DATA: wa_return LIKE zmme_return_ordem_servico,
      it_return LIKE STANDARD TABLE OF wa_return.

*&---------------------------------------------------------------------*
*& Tipos
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_saida,
    bukrs  TYPE coep-bukrs,
    gjahr  TYPE coep-gjahr,
    perio  TYPE coep-perio,
    kstar  TYPE coep-kstar,
    wogbtr TYPE coep-wogbtr,
    wkgbtr TYPE coep-wkgbtr,
    mbgbtr TYPE coep-mbgbtr,
*    MEINB  TYPE COEP-MEINB,
*    PAROB1 TYPE COEP-PAROB1,
    gsber  TYPE coep-gsber,
    objnr  TYPE coep-objnr,
*    BELNR  TYPE COEP-BELNR,
*    BUZEI  TYPE COEP-BUZEI,
*    OBJNR2 TYPE COEP-OBJNR,
  END   OF ty_saida.

*&---------------------------------------------------------------------*
*& Tabelas Internas
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_coep.
         INCLUDE STRUCTURE coep.
TYPES:   objnr2 TYPE coep-objnr,
       END OF ty_coep.

DATA : it_setleaf    TYPE TABLE OF setleaf,
       it_coas       TYPE TABLE OF coas,
       it_coep       TYPE TABLE OF ty_coep,
       it_caufv      TYPE TABLE OF caufv,
       it_afvc       TYPE TABLE OF afvc,

*       IT_COEP_AUX     TYPE TABLE OF TY_COEP,
*       IT_COEP_SOV     TYPE TABLE OF TY_COEP,
*       IT_COEP_SOV_AUX TYPE TABLE OF TY_COEP,
       it_coas_objnr TYPE TABLE OF coas,
       it_saida      TYPE TABLE OF ty_saida.

*&---------------------------------------------------------------------*
*& WORKAREAS
*&---------------------------------------------------------------------*

DATA : wa_setleaf      TYPE setleaf,
       wa_coas         TYPE coas,
       wa_coep         TYPE ty_coep,

       wa_caufv        TYPE caufv,
       wa_afvc         TYPE afvc,

       wa_coep_aux     TYPE ty_coep,
       wa_coep_sov     TYPE ty_coep,
       wa_coep_sov_aux TYPE ty_coep,
       wa_coas_objnr   TYPE coas,
       wa_saida        TYPE ty_saida.


*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR coep-bukrs NO-EXTENSION NO INTERVALS OBLIGATORY ,
                  p_gjahr FOR coep-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY ,
                  p_perio FOR coep-perio NO-EXTENSION NO INTERVALS OBLIGATORY .
SELECTION-SCREEN END   OF BLOCK b1.

*SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*PARAMETERS:
*             P_DEL  AS CHECKBOX.
*SELECTION-SCREEN: END OF BLOCK B2.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM selecionar_dados.
  PERFORM organizacao_dados.
  " PERFORM ENVIAR_DADOS.

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  DATA: v_perio     TYPE coep-perio,
        v_month(2)  TYPE c,
        qt_material TYPE coep-mbgbtr,
        qt_valor    TYPE coep-wogbtr,
        qt_valor1   TYPE coep-wkgbtr,
        tabix       TYPE sy-tabix.


  RANGES:  rg_gsber FOR coas-gsber,
           rg_objnr FOR coep-objnr,
           rg_beknz FOR coep-beknz.

  REFRESH: rg_gsber,
           rg_objnr,
           rg_beknz.


  v_month =  sy-datum+4(2) .                                "20131211

*  V_PERIO = ( V_MONTH - 1 ).
  v_perio = ( v_month - 2 ).

*==========================================Inicio IR080356  - Anderson Oenning

*  IF  P_PERIO-LOW < V_PERIO .
*    MESSAGE I000(Z01) WITH 'Informar mês atual ou anterior. !'.
*    STOP.
*  ENDIF.

*==========================================Inicio IR080356 -

  " Busca Contas SET
  SELECT *
     FROM setleaf
     INTO TABLE it_setleaf
  WHERE setname EQ 'MAGGI_CENTROS_MODULO_PM'.


*  IF ( P_DEL EQ 'X' ).
*
*    PERFORM DELETA_REGISTROS.
*
*  ELSE.

  LOOP AT it_setleaf INTO wa_setleaf.
    rg_gsber-sign   = 'I'.
    rg_gsber-option = 'EQ'.
    rg_gsber-low    = wa_setleaf-valfrom.
    APPEND rg_gsber.
    CLEAR: rg_gsber.
  ENDLOOP.

  SELECT *
    INTO TABLE it_coas
    FROM coas
   WHERE  autyp IN ( '30', '01' )
     AND bukrs IN p_bukrs
     AND gsber IN rg_gsber.

  IF it_coas IS NOT INITIAL.

    SELECT *
      FROM caufv
      INTO TABLE  it_caufv
      FOR ALL ENTRIES IN it_coas
      WHERE aufnr = it_coas-aufnr.

    IF it_caufv[] IS NOT INITIAL.
      SELECT *
        FROM afvc
        INTO TABLE it_afvc
        FOR ALL ENTRIES IN it_caufv
        WHERE aufpl EQ it_caufv-aufpl.

    ENDIF.

    SELECT *
      INTO TABLE it_coep
      FROM coep
    WHERE gjahr IN p_gjahr
      AND perio IN p_perio.

    "FF - inicio - 14.12.2023
    IF it_coep[] IS NOT INITIAL.

      SELECT ebeln, ebelp, belnr
      FROM ekbe
      INTO TABLE @DATA(lt_ekbe)
      FOR ALL ENTRIES IN @it_coep
      WHERE gjahr = @it_coep-gjahr
        and ebeln = @it_coep-ebeln
        AND ebelp = @it_coep-ebelp
        AND bewtp = 'A'. "Adiantamento

*      IF sy-subrc = 0 AND lt_ekbe[] IS NOT INITIAL.
*
*        SELECT bukrs, gjahr, augbl
*        INTO TABLE @DATA(lt_bsak)
*        FROM bsak_view FOR ALL ENTRIES IN @lt_ekbe
*        WHERE augbl = @lt_ekbe-belnr
*          AND umsks = 'A'. "Adiantamento
*
*      ENDIF.
    ENDIF.
    "FF - fim - 14.12.2023


    rg_beknz-sign   = 'I'.
    rg_beknz-option = 'EQ'.
    rg_beknz-low    = 'A'.
    APPEND  rg_beknz.

    DELETE it_coep WHERE bukrs NOT IN p_bukrs.
    DELETE it_coep WHERE beknz IN rg_beknz.

*    IT_COAS_OBJNR[] = IT_COAS[].
*    SORT IT_COAS_OBJNR BY OBJNR.
*    DELETE ADJACENT DUPLICATES FROM IT_COAS_OBJNR COMPARING OBJNR.
*    RG_OBJNR-SIGN = 'I'.
*    RG_OBJNR-OPTION = 'EQ'.
*
*    LOOP AT  IT_COAS_OBJNR INTO WA_COAS_OBJNR.
*      MOVE WA_COAS_OBJNR-OBJNR TO RG_OBJNR-LOW.
*      APPEND RG_OBJNR.
*      CLEAR WA_COAS_OBJNR.
*    ENDLOOP.

*    DELETE IT_COEP WHERE OBJNR NOT IN RG_OBJNR.

    SORT: it_afvc  BY objnr,
          it_caufv BY aufpl.

    LOOP AT it_coep INTO wa_coep.

      tabix = sy-tabix.

      DATA(lv_tabix_coep) = sy-tabix.

      "FF - inicio - 14.12.2023
      READ TABLE lt_ekbe INTO DATA(wa_ekbe) WITH KEY ebeln = wa_coep-ebeln
                                                     ebelp = wa_coep-ebelp.

      IF sy-subrc = 0. "Se encontrar, não enviar o registro, pois é um documento de adiantamento.
        DELETE it_coep INDEX lv_tabix_coep.
      ENDIF.

    "FF - fim - 14.12.2023


    IF wa_coep-objnr+0(2) NE 'OR'. "novo tipo
      READ TABLE it_afvc INTO wa_afvc WITH KEY objnr = wa_coep-objnr BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_caufv INTO wa_caufv WITH KEY aufpl  = wa_afvc-aufpl BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE wa_coep-objnr  TO wa_coep-objnr2.
          MOVE wa_caufv-objnr TO wa_coep-objnr.
          MODIFY it_coep FROM wa_coep INDEX tabix TRANSPORTING objnr objnr2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*    DELETE IT_COEP WHERE OBJNR NOT IN RG_OBJNR.

  "Totalizar por GJHAR / PERIO / GSBER / OBJNR  / KSTAR
*    IT_COEP_AUX[] = IT_COEP[].

  SORT: "IT_COEP_AUX BY GJAHR PERIO GSBER  OBJNR KSTAR,
        it_coep     BY gjahr perio gsber  objnr kstar.

*    DELETE ADJACENT DUPLICATES FROM IT_COEP_AUX COMPARING GJAHR PERIO GSBER  OBJNR KSTAR.

  SORT it_coas BY objnr.

  LOOP AT it_coep INTO wa_coep.
    READ TABLE it_coas INTO wa_coas WITH KEY objnr = wa_coep-objnr BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

*      LOOP AT IT_COEP INTO WA_COEP WHERE GJAHR EQ WA_COEP_AUX-GJAHR AND PERIO EQ WA_COEP_AUX-PERIO
*        AND GSBER EQ WA_COEP_AUX-GSBER AND OBJNR EQ WA_COEP_AUX-OBJNR AND KSTAR EQ WA_COEP_AUX-KSTAR .
*
*        QT_MATERIAL = QT_MATERIAL +  WA_COEP-MBGBTR.
*        QT_VALOR    = QT_VALOR    +  WA_COEP-WOGBTR.
*        QT_VALOR1   = QT_VALOR1   +  WA_COEP-WKGBTR.
*      ENDLOOP.

    wa_saida-bukrs  =  wa_coep-bukrs.
    wa_saida-gjahr  =  wa_coep-gjahr.
    wa_saida-perio  =  wa_coep-perio.
    wa_saida-kstar  =  wa_coep-kstar.
    wa_saida-wogbtr =  wa_coep-wogbtr. "QT_VALOR.
    wa_saida-wkgbtr =  wa_coep-wkgbtr. "QT_VALOR1.
    wa_saida-mbgbtr =  wa_coep-mbgbtr. "QT_MATERIAL.

*      WA_SAIDA-MEINB  =  WA_COEP-MEINB.
*      WA_SAIDA-PAROB1 =  WA_COEP-PAROB1.
    wa_saida-gsber  =  wa_coep-gsber.
    wa_saida-objnr  =  wa_coep-objnr.
*      WA_SAIDA-BELNR  =  WA_COEP-BELNR.
*      WA_SAIDA-BUZEI  =  WA_COEP-BUZEI.
*      WA_SAIDA-OBJNR2 =  WA_COEP-OBJNR2.

    COLLECT wa_saida  INTO it_saida.

    CLEAR: wa_saida, wa_coep, qt_material, qt_valor, qt_valor1 .

  ENDLOOP.

*    " Consumo sem ordem PM
*    SELECT *
*      INTO TABLE IT_COEP_SOV
*      FROM COEP
*    WHERE  BUKRS IN P_BUKRS
*      AND  GJAHR IN P_GJAHR
*      AND  PERIO IN P_PERIO
*      AND  GKOAR EQ 'M'.
*
*    IT_COEP_SOV_AUX[] = IT_COEP_SOV[].
*    CLEAR: IT_COEP_SOV.
*
*    LOOP AT IT_COEP_SOV_AUX INTO WA_COEP_SOV_AUX .
*      IF WA_COEP_SOV_AUX-OBJNR(6) = 'KSMAGI'.
*
*        MOVE-CORRESPONDING WA_COEP_SOV_AUX TO WA_COEP_SOV.
*        APPEND WA_COEP_SOV TO IT_COEP_SOV.
*
*        CLEAR: WA_COEP_SOV_AUX, WA_COEP_SOV.
*
*      ENDIF.
*    ENDLOOP.
*
*    "Totalizar por GJHAR / PERIO / GSBER / OBJNR  / KSTAR ///
*    IT_COEP_SOV_AUX[] = IT_COEP_SOV[].
*
*    SORT: IT_COEP_SOV_AUX BY GJAHR PERIO GSBER  OBJNR KSTAR,
*          IT_COEP_SOV     BY GJAHR PERIO GSBER  OBJNR KSTAR.
*
*    DELETE ADJACENT DUPLICATES FROM IT_COEP_SOV_AUX COMPARING GJAHR PERIO GSBER  OBJNR KSTAR.
*
*    LOOP AT IT_COEP_SOV_AUX INTO WA_COEP_SOV_AUX .
*
**      LOOP AT IT_COEP_SOV INTO WA_COEP_SOV WHERE GJAHR EQ WA_COEP_SOV_AUX-GJAHR AND PERIO EQ WA_COEP_SOV_AUX-PERIO
**        AND GSBER EQ WA_COEP_SOV_AUX-GSBER AND OBJNR EQ WA_COEP_SOV_AUX-OBJNR AND KSTAR EQ WA_COEP_SOV_AUX-KSTAR .
**
**        QT_MATERIAL = QT_MATERIAL +  WA_COEP_SOV-MBGBTR.
**        QT_VALOR    = QT_VALOR    +  WA_COEP_SOV-WOGBTR.
**        QT_VALOR1   = QT_VALOR1   +  WA_COEP_SOV-WKGBTR.
**
**      ENDLOOP.
*
*      WA_SAIDA-BUKRS  =  WA_COEP_SOV-BUKRS.
*      WA_SAIDA-GJAHR  =  WA_COEP_SOV-GJAHR.
*      WA_SAIDA-PERIO  =  WA_COEP_SOV-PERIO.
*      WA_SAIDA-KSTAR  =  WA_COEP_SOV-KSTAR.
*      WA_SAIDA-WOGBTR =  WA_COEP_SOV-WOGBTR. "QT_VALOR.
*      WA_SAIDA-WKGBTR =  WA_COEP_SOV-WKGBTR. "QT_VALOR1.
*      WA_SAIDA-MBGBTR =  WA_COEP_SOV-MBGBTR. "QT_MATERIAL.
**      WA_SAIDA-MEINB  =  WA_COEP_SOV-MEINB.
**      WA_SAIDA-PAROB1 =  WA_COEP_SOV-PAROB1.
*      WA_SAIDA-GSBER  =  WA_COEP_SOV-GSBER.
*      WA_SAIDA-OBJNR  =  WA_COEP_SOV-OBJNR.
**      WA_SAIDA-BELNR  =  WA_COEP_SOV-BELNR.
**      WA_SAIDA-BUZEI  =  WA_COEP_SOV-BUZEI.
*
*      COLLECT WA_SAIDA  INTO IT_SAIDA.
*
*      CLEAR: WA_SAIDA, WA_COEP_SOV, QT_MATERIAL, QT_VALOR, QT_VALOR1 .
*
*    ENDLOOP.

ELSE.
  MESSAGE i000(z01) WITH 'Não foram encontrados dados!'.
  STOP.
ENDIF.
"  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizacao_dados .

  DATA: v_txt50    TYPE skat-txt50,
        v_hora     TYPE sy-uzeit,
        v_data_i   TYPE sy-datum,
        v_data_f   TYPE sy-datum,
        v_sort     TYPE cobra-sort,
        vl_setname TYPE setleaf-setname.


  CONCATENATE  p_gjahr-low  p_perio-low+1  '01' INTO v_data_i.


  " Pego o Final do mes
  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = v_data_i
    IMPORTING
      last_day_of_month = v_data_f.


  v_hora = sy-uzeit.



  SORT it_coas BY objnr.

  DELETE it_saida WHERE wogbtr = 0 AND wkgbtr = 0.

  "DELETE TABLE IT_SAIDA WITH TABLE KEY K1 = V1 ... KN = VN.


  LOOP AT it_saida INTO wa_saida .

    READ TABLE it_coas INTO wa_coas WITH KEY objnr = wa_saida-objnr BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR v_txt50.

    SELECT SINGLE a~txt50
      INTO v_txt50
     FROM skat AS a INNER JOIN ska1 AS b "#EC CI_DB_OPERATION_OK[2431747]
      ON b~saknr = a~saknr             "#EC CI_DB_OPERATION_OK[2389136]
      WHERE a~saknr = wa_saida-kstar
      AND   spras = sy-langu.


      wa_return-no_os_number       =  wa_coas-aufnr.   " NO_OS
      wa_return-aufnr              =  ''           .   " NO_ORDEM_ERP
      wa_return-erdat              =  v_data_f.        " DT_REFER
      wa_return-erzet              =  v_hora.          " DT_REFER
      wa_return-docmat             =  wa_saida-bukrs.  " NO_REQUIS_ERP
      wa_return-buzei              =  ''.              " NO_SEQUENCIA

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = wa_saida-kstar
        IMPORTING
          output = wa_return-matnr.

      "WA_RETURN-MATNR              =  WA_SAIDA-KSTAR.  " CD_MATERIAL_ERP

      wa_return-maktx              =  v_txt50.         " DE_MATERIAL_ERP
      wa_return-hkont              =  wa_saida-kstar.  " CD_CTA_CON

      SELECT SINGLE setname
        FROM setleaf
        INTO vl_setname
       WHERE setname = 'MAGGI_CONTAS_PM_COMBUST'
         AND valfrom = wa_saida-kstar.

        IF sy-subrc IS INITIAL.
*      wa_return-menge             =  wa_saida-mbgbtr. " QT_MATERIAL // FF 29.01.24 - IR168435
          wa_return-menge              =  abs( wa_saida-mbgbtr ). " QT_MATERIAL // FF 29.01.24 - IR168435
          wa_return-dmbtr              =  abs( wa_saida-wogbtr ). " QT_VALOR
          wa_return-dmbe2              =  abs( wa_saida-wkgbtr ). " QT_VALOR1
        ELSE.
          wa_return-menge              =  1.
          wa_return-dmbtr              =  abs( wa_saida-wogbtr ). " QT_VALOR
          wa_return-dmbe2              =  abs( wa_saida-wkgbtr ). " QT_VALOR1
        ENDIF.

        wa_return-dmbe3                =  ''.              " QT_VALOR2
        IF wa_saida-wogbtr < 0 .
          wa_return-shkzg              =  2.                "FG_MOVTO  " Negativo 2
        ELSE.
          wa_return-shkzg              =  1.                "FG_MOVTO  " Positivo 1
        ENDIF.

        wa_return-gsber              =  wa_saida-gsber. " CD_FILIAL_ERP
        wa_return-bukrs              =  wa_saida-bukrs. " CD_EMPRESA_ERP

        CLEAR : v_sort.
        SELECT SINGLE a~sort
          INTO v_sort
        FROM cobra AS a
        WHERE objnr = wa_saida-objnr.
          IF sy-subrc NE 0.
*      SELECT SINGLE A~SORT
*           INTO V_SORT
*             FROM COBRA AS A
*             WHERE OBJNR = WA_SAIDA-OBJNR2.
*      IF SY-SUBRC NE 0.
            CONCATENATE 'KSMAGI'  wa_coas-kostl INTO v_sort.
*      ENDIF.
          ENDIF.

          wa_return-kostl              =  v_sort+6(10). "CD_CCUSTO "(SOMENTE NÚMEROS)  KSMAGI0150210071   SY-DATUM+4(2) . "20131211

          APPEND wa_return TO it_return.

          CLEAR: wa_return, wa_coas.

        ENDLOOP.

        PERFORM: f_rfc_envio.


ENDFORM.                    " ORGANIZACAO_DADOS

*&---------------------------------------------------------------------*
*&      Form  DELETA_REGISTROS
*&---------------------------------------------------------------------*
FORM deleta_registros .

  DATA: data_inicial TYPE sy-datum,
        data_final   TYPE sy-datum,
        hora         TYPE sy-uzeit.



  CALL FUNCTION 'OIL_MONTH_GET_FIRST_LAST'
    EXPORTING
      i_month     = p_perio-low+1
      i_year      = p_gjahr-low
    IMPORTING
      e_first_day = data_inicial      "I_DATE = S_BUDAT-HIGH
      e_last_day  = data_final
    EXCEPTIONS
      wrong_date  = 1
      OTHERS      = 2.

  IF ( sy-subrc EQ 0 ).

    REFRESH: it_return.
    CLEAR:   wa_return.

    hora = sy-uzeit.


    CLEAR wa_setleaf.


    LOOP AT it_setleaf INTO wa_setleaf.


      wa_return-gsber       =  wa_setleaf-valfrom.
      wa_return-docmat      =  p_bukrs-low. "NO_REQUIS_ERP


      wa_return-erdat = data_final. " DT_REFER
      wa_return-erzet = hora.       " DT_REFER

      wa_return-kostl = '9999999999'.
      wa_return-hkont = '0'.
      wa_return-dmbtr = '0'.


      APPEND wa_return TO it_return.

    ENDLOOP.



    PERFORM f_rfc_envio.

  ENDIF.
ENDFORM.                    " DELETA_REGISTROS

*&---------------------------------------------------------------------*
*&      Form  f_rfc_despesas
*&---------------------------------------------------------------------*
FORM f_rfc_envio .

  DATA: qtd TYPE sy-tabix.
* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*  CALL FUNCTION 'Z_MM_OUTBOUND_ORDEM_SERVICO' IN BACKGROUND TASK
*    DESTINATION 'XI_GEO_ORDEM_SERVICO'
*    AS SEPARATE UNIT
*    TABLES
*      return_sucess = it_return.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_ORDEM_SERVICO'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        return_sucess = it_return.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        return_sucess = it_return.
  ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim

  COMMIT WORK.

  CLEAR: qtd.
  DESCRIBE TABLE it_return LINES qtd.


  IF sy-subrc EQ 0.
    MESSAGE s000(z01) WITH qtd 'Registro(s) Enviados com Sucesso. '.
  ENDIF.



ENDFORM.                    " f_rfc_despesas
