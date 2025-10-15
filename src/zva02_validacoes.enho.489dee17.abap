"Name: \PR:SAPMV45A\FO:USEREXIT_DELETE_DOCUMENT\SE:BEGIN\EI
ENHANCEMENT 0 ZVA02_VALIDACOES.
CASE sy-tcode.
  WHEN 'VA02' OR 'ZMEMO00'.

    IF vbak-vkorg NE '0100'.

      DATA: wa_0001 TYPE zsdt0001.

      SELECT SINGLE *
        FROM zsdt0001
        INTO wa_0001
          WHERE vbeln EQ vbak-vbeln.

      IF sy-subrc IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ordem possui Romaneio' wa_0001-nr_romaneio 'Vinculado!'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
    ENDIF.
ENDCASE.

***MAGGI/Victor - Início da Inclusão - 12.06.2012
TABLES: zfit0026.

TYPES: BEGIN OF ty_bsid1,
         bukrs TYPE bsid-bukrs,
         kunnr TYPE bsid-kunnr,
         vbel2 TYPE bsid-vbel2,
       END OF ty_bsid1,

       BEGIN OF ty_bsad1,
         bukrs TYPE bsad-bukrs,
         kunnr TYPE bsad-kunnr,
         vbel2 TYPE bsad-vbel2,
       END OF ty_bsad1.

DATA: lt_bsid TYPE TABLE OF ty_bsid1,
      lt_bsad TYPE TABLE OF ty_bsad1,
      lt_0041 TYPE TABLE OF zsdt0041 WITH HEADER LINE,
      v_msg   TYPE c LENGTH 255.

DATA: lw_zfit0026 TYPE zfit0026.

DATA: lv_vbeln   TYPE p,
      lv_vbeln_c TYPE string.

CALL FUNCTION 'MOVE_CHAR_TO_NUM'
  EXPORTING
    chr = vbak-vbeln
  IMPORTING
    num = lv_vbeln.

lv_vbeln_c = lv_vbeln.

SELECT a~bukrs a~kunnr a~vbel2
  INTO TABLE lt_bsid
  FROM bsid AS a
  INNER JOIN bkpf AS b ON a~bukrs = b~bukrs
                      AND a~belnr = b~belnr
                      AND a~gjahr = b~gjahr
WHERE a~bukrs EQ vbak-vkorg
  AND a~kunnr EQ vbak-kunnr
  AND b~xreversal EQ ''
  AND ( ( a~vbel2 EQ vbak-vbeln ) OR
        ( a~xblnr EQ vbak-vbeln ) OR
        ( a~vbel2 EQ lv_vbeln_c ) OR
        ( a~xblnr EQ lv_vbeln_c )  ).

SELECT a~bukrs a~kunnr a~vbel2
  INTO TABLE lt_bsad
  FROM bsad AS a
  INNER JOIN bkpf AS b ON a~bukrs = b~bukrs
                      AND a~belnr = b~belnr
                      AND a~gjahr = b~gjahr
 WHERE a~bukrs EQ vbak-vkorg
   AND a~kunnr EQ vbak-kunnr
   AND b~xreversal EQ ''
   AND ( ( a~vbel2 EQ vbak-vbeln ) OR
         ( a~xblnr EQ vbak-vbeln ) OR
         ( a~vbel2 EQ lv_vbeln_c ) OR
         ( a~xblnr EQ lv_vbeln_c )  ).

IF ( lt_bsid[] IS NOT INITIAL ) OR ( lt_bsad[] IS NOT INITIAL ).
  MESSAGE i836(sd) WITH 'Ordem de venda não pode ser eliminada,' 'existem Doc. Contábeis Vinculados!'.
  LEAVE TO CURRENT TRANSACTION.
ENDIF.

*  SELECT BUKRS KUNNR VBEL2
*    FROM BSID
*    INTO TABLE IT_BSID
*  WHERE BUKRS EQ VBAK-VKORG
*    AND KUNNR EQ VBAK-KUNNR
*    AND VBEL2 EQ VBAK-VBELN.
*
*  IF NOT ( IT_BSID[] IS INITIAL ).
*
*    SELECT BUKRS KUNNR VBEL2
*      FROM BSAD
*      INTO TABLE IT_BSAD
*      FOR ALL ENTRIES IN IT_BSID
*    WHERE BUKRS EQ IT_BSID-BUKRS
*      AND KUNNR EQ IT_BSID-KUNNR
*      AND VBEL2 EQ IT_BSID-VBEL2.
*
*    IF ( SY-SUBRC EQ 0 ).
*      MESSAGE I836(SD) WITH 'Ordem de venda não pode ser eliminada,' 'existe adiantamento vinculado,' ' precisa transferir o mesmo'.
*      LEAVE TO CURRENT TRANSACTION.
*    ENDIF.
*  ENDIF.


*** Verificar se a OV já possui documento subsequente gerado pelo Cockpit de INSUMOS (ZSDT0087) 02/02/18
**Inicio
IF sy-tcode EQ 'VA02' OR sy-tcode EQ 'ZMEMO00'.
  DATA: lt_count TYPE i,
        lt_cont  TYPE i VALUE 0.

  SELECT COUNT( DISTINCT vbeln )
         FROM zsdt0090
         INTO lt_count
         WHERE vbelv EQ vbak-vbeln
          AND  estorno NE 'X'.

  ADD lt_count TO lt_cont.

  SELECT COUNT( DISTINCT vbeln )
         FROM zsdt0090
         INTO lt_count
         WHERE vbeln EQ vbak-vbeln
          AND  estorno NE 'X'.

  ADD lt_count TO lt_cont.

  IF lt_cont > 0.
    MESSAGE i836(sd) WITH 'Ordem de venda está associada a outra,' 'só pode ser eliminada pelo COCKPIT(ZSDT0087).'.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.
ENDIF.

**FIM


*{   INSERT         DEVK908189                                        1
***BBKO/André Zorba - Início da Inclusão - 23.07.2010
* Chamar evento de encerramento do workflow atual caso
* a O.V seja excluída.
* O WF de Limite de Crédito contém uma espera por evento
* para encerrar o fluxo correspondente.

* Constantes
CONSTANTS: c_error TYPE char1 VALUE 'E'.

* Faz a chamada do evento de encerramento.
CALL FUNCTION 'Z_SD_WF_CHAMAR_EVENTOS'
  EXPORTING
    vbeln       = vbak-vbeln
    tipo_evento = c_error.

* Retorno de Formação Lote - Inicio
DATA: lt_tvarvc TYPE TABLE OF tvarvc,
      lt_export TYPE TABLE OF zsdt_export.

* Retorno de Formação Lote - Fim

CLEAR vbak-tknum.

IF sy-tcode EQ 'VA02' OR sy-tcode EQ 'ZMEMO00'.

**  Marcos Faneli --> CH.126564 - 04.06.2014
  DATA: lv_count TYPE i,
        lv_cont  TYPE i VALUE 0.

  SELECT COUNT( DISTINCT vbeln )
         FROM zsdt0053
         INTO lv_count
         WHERE vbeln EQ vbak-vbeln AND
              status NE 'C'.

  ADD lv_count TO lv_cont.

  SELECT COUNT( DISTINCT vbeln )
         FROM zsdt0100
         INTO lv_count
         WHERE vbeln EQ vbak-vbeln AND
              status NE 'C'.

  ADD lv_count TO lv_cont.

  SELECT COUNT( DISTINCT vbeln )
         FROM zsdt0066
         INTO lv_count
         WHERE vbeln = vbak-vbeln.

  ADD lv_count TO lv_cont.

  IF lv_cont > 0.
    MESSAGE i836(sd) WITH 'Ordem de venda só pode ser eliminada pelo COCKPIT.'.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.

ENDIF.

"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo
"Vai executar não colocar validações abaixo desse ponto, pois tem que ser atômico o processo

***MAGGI/Victor - Fim da Inclusão - 12.06.2012

***MAGGI/Victor - Início da Inclusão - 26.08.2013
CLEAR: lw_zfit0026.
SELECT SINGLE * FROM zfit0026 INTO lw_zfit0026 WHERE vbeln EQ vbak-vbeln.

IF ( sy-subrc EQ 0 ).
  UPDATE zfit0026 SET eliminado = 'X' WHERE vbeln EQ vbak-vbeln.
ENDIF.
***MAGGI/Victor - Fim da Inclusão - 26.08.2013

***BBKO/André Zorba - Fim da Inclusão - 23.07.2010

*}   INSERT

* Vendas Frame - Inicio
IF sy-tcode EQ 'VA01' OR
   sy-tcode EQ 'VA02' OR
   sy-tcode EQ 'ZMEMO00'.

  IF vbak-auart EQ 'ZFRM'.
    UPDATE zsdt0021
       SET ordem_venda = space
     WHERE vbeln       EQ cvbak-vbeln
       AND dta_preco   EQ cvbak-erdat
       AND ordem_venda EQ vbak-vbeln.
  ENDIF.

ENDIF.
* Vendas Frame - Fim

IF fcode EQ 'LOES'.
  IF  ( sy-ucomm EQ 'YES'  AND sy-tcode EQ 'VA02' ) OR call_bapi = 'X'.

    SELECT  *
      FROM zsdt0041
      INTO TABLE lt_0041
       WHERE vbeln EQ vbak-vbeln.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_0041.

        CLEAR: lt_0041-vbeln.
        MODIFY zsdt0041 FROM lt_0041.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDIF.

**  Marcos Faneli -> Remover referência na ZNOM_PROGRAMACAO Ch.130025 - 11.12.2014
IF sy-tcode EQ 'VA02' OR sy-tcode EQ 'ZMEMO00'.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-index
      text       = 'Buscando vinculos com memorandos ...'.

  SELECT COUNT( DISTINCT nr_ordem )
    FROM znom_remetente
    INTO lv_count
    WHERE nr_ordem = vbak-vbeln.

  IF lv_count IS NOT INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-index
        text       = 'Removendo vinculos com memorandos ...'.

    UPDATE znom_remetente SET nr_ordem = '' WHERE nr_ordem = vbak-vbeln.
  ENDIF.
ENDIF.

**  Marcos Faneli -> Remover referência na ZNOM_REMETENTE Ch.130025 - 11.12.2014
IF ( sy-tcode EQ 'VA42' ).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-index
      text       = 'Buscando vinculos com memorandos ...'.

  SELECT COUNT( DISTINCT contrato )
    FROM znom_programacao
    INTO lv_count
   WHERE contrato = vbak-vbeln.

  IF ( lv_count IS NOT INITIAL ).
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-index
        text       = 'Removendo vinculos com memorandos ...'.

    UPDATE znom_programacao SET contrato = '' WHERE contrato = vbak-vbeln.
  ENDIF.
ENDIF.

IF sy-tcode EQ 'VA01' OR
   sy-tcode EQ 'VA02' OR
   sy-tcode EQ 'ZSDT0066' OR
   sy-tcode EQ 'ZLES0077' OR
   sy-tcode EQ 'ZMEMO00'.

  SELECT *
    FROM tvarvc
    INTO TABLE lt_tvarvc
   WHERE name EQ 'VENDA_EXPORT_LOTE'.
  SORT lt_tvarvc BY low ASCENDING.

  READ TABLE lt_tvarvc
    WITH KEY low = vbak-auart
    BINARY SEARCH
    TRANSPORTING NO FIELDS.

  IF sy-subrc IS INITIAL.
    UPDATE zsdt_export
       SET ordem  = space
           export = space
     WHERE ordem  = vbak-vbeln
       AND export = 'X'.
  ENDIF.

ENDIF.

IF sy-tcode EQ 'VA01' OR
   sy-tcode EQ 'VA02' OR
   sy-tcode EQ 'VA41' OR
   sy-tcode EQ 'ZSDT0066' OR
   sy-tcode EQ 'ZSDT0062' OR
   sy-tcode EQ 'ZLES0077' OR
   sy-tcode EQ 'ZSDT0087' OR
   sy-tcode EQ 'ZSDT0081' OR  "// US-169490 WBARBOSA 24/07/2025
   sy-tcode EQ 'ZSDT0123' OR
   sy-tcode EQ 'VA42' OR
   sy-tcode EQ 'ZMEMO00'.

  DATA: lw_vbap TYPE vbap,
        lw_vbkd TYPE vbkd,
        lw_vbpa TYPE vbpa,
        lt_vbpa TYPE TABLE OF vbpa.

  APPEND lw_vbpa TO lt_vbpa.

  DATA(lwa_fert_prod) = space.

*--> 24.08.2023 18:38:34 - Migração S4 – ML - Início
**   Atualiza Legado
*    CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA'
*      IN BACKGROUND TASK DESTINATION 'XI_ORDEM_VENDA'
*      EXPORTING
*        NU_ORDEM_VENDA = VBAK-VBELN
*        TP_ORDEM_VENDA = VBAK-AUART
*        NU_ITEM        = VBAP-POSNR
*        DT_ORDEM_VENDA = VBAK-ERDAT
*        TP_FRETE       = VBKD-INCO1
*        ID_CLIENTE     = VBAK-KUNNR
*        CD_MATERIAL    = VBAP-MATNR
*        CD_SAFRA       = VBAP-CHARG
*        CD_EMPRESA     = VBAK-VKORG
*        CD_CENTRO      = VBAP-WERKS
*        CD_MOEDA       = VBAP-WAERK
*        STATUS         = 'D'
*        DT_ATUALIZACAO = SY-DATUM
*        HR_ATUALIZACAO = SY-UZEIT
*        TRANSGENIA     = VBAK-KVGR3
*        ID_LOTE_FRETE  = VBAP-ID_LOTE_FRETE
*        FERT_PRODUCAO  = lwa_fert_prod
*     TABLES
*        IT_VBPA        = LT_VBPA.

  "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
  zcl_eudr_utils=>check_ov_pedido_eudr(
    EXPORTING
      i_vbeln          = vbak-vbeln     " Nº do documento de compras
      i_ck_lgort_param = abap_true
      i_lgort          = vbap-lgort
    RECEIVING
      r_eudr  =   DATA(v_eudr)               " Atende critérios Europeu
  ).
  "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

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
      EXPORTING
        nu_ordem_venda = vbak-vbeln
        tp_ordem_venda = vbak-auart
        nu_item        = vbap-posnr
        dt_ordem_venda = vbak-erdat
        tp_frete       = vbkd-inco1
        id_cliente     = vbak-kunnr
        cd_material    = vbap-matnr
        cd_safra       = vbap-charg
        cd_empresa     = vbak-vkorg
        cd_centro      = vbap-werks
        cd_moeda       = vbap-waerk
        status         = 'D'
        dt_atualizacao = sy-datum
        hr_atualizacao = sy-uzeit
        transgenia     = vbak-kvgr3
        id_lote_frete  = vbap-id_lote_frete
        fert_producao  = lwa_fert_prod
        tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
      TABLES
        it_vbpa        = lt_vbpa.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      EXPORTING
        nu_ordem_venda = vbak-vbeln
        tp_ordem_venda = vbak-auart
        nu_item        = vbap-posnr
        dt_ordem_venda = vbak-erdat
        tp_frete       = vbkd-inco1
        id_cliente     = vbak-kunnr
        cd_material    = vbap-matnr
        cd_safra       = vbap-charg
        cd_empresa     = vbak-vkorg
        cd_centro      = vbap-werks
        cd_moeda       = vbap-waerk
        status         = 'D'
        dt_atualizacao = sy-datum
        hr_atualizacao = sy-uzeit
        transgenia     = vbak-kvgr3
        id_lote_frete  = vbap-id_lote_frete
        fert_producao  = lwa_fert_prod
        tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
      TABLES
        it_vbpa        = lt_vbpa.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 18:38:34 - Migração S4 – ML – Fim

ENDIF.

DATA: i_ordem_venda      TYPE zde_cargueiro_ov,
      matkl_fertilizante TYPE RANGE OF matkl,
      w_mara             TYPE mara,
      t_matkl_fert       TYPE TABLE OF tvarvc.

*-CS2020001303 - 10.11.2021 - JT - inicio
*-----------------
*-- Grupo fertilizantes
*-----------------
FREE: matkl_fertilizante.

SELECT *
  FROM tvarvc
  INTO TABLE t_matkl_fert
 WHERE name = 'MAGGI_GR_FERTILIZANTES'.

IF sy-subrc = 0.
  LOOP AT t_matkl_fert INTO DATA(w_matkl_fert).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_fert-low ) TO matkl_fertilizante.
  ENDLOOP.
ENDIF.
*-CS2020001303 - 10.11.2021 - JT - fim

LOOP AT xvbap INTO DATA(wa_xvbap).

  CLEAR: i_ordem_venda.

*-CS2020001303 - 10.11.2021 - JT - inicio
  CLEAR w_mara.
  SELECT SINGLE *
    INTO w_mara
    FROM mara
   WHERE matnr = wa_xvbap-matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = w_mara-matkl
    IMPORTING
      output = w_mara-matkl.

  IF w_mara-matkl IN matkl_fertilizante[] AND matkl_fertilizante[] IS NOT INITIAL.
    CONTINUE.
  ENDIF.
*-CS2020001303 - 10.11.2021 - JT - fim

  MOVE-CORRESPONDING vbak TO i_ordem_venda.
  i_ordem_venda-item = wa_xvbap.
  i_ordem_venda-parceiros = xvbpa[].
  i_ordem_venda-comercial = xvbkd[].

  IF wa_xvbap-id_lote_frete IS NOT INITIAL.
    TRY .
        zcl_integracao_lote_frete=>zif_integracao_lote_frete~get_instance(
         )->set_registro( EXPORTING i_id_lote_frete = wa_xvbap-id_lote_frete i_ordem_venda = i_ordem_venda
         )->set_delete_lote_frete(
         ).
      CATCH zcx_integracao. " .
      CATCH zcx_error. " .
    ENDTRY.
  ENDIF.
ENDLOOP.

* EXIT. "*-CS2019001753-04.07.2023-#65741
ENDENHANCEMENT.
