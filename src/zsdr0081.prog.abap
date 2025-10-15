*&---------------------------------------------------------------------*
*& Report  ZSDR0081
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0081.

TABLES: vbrk, vbrp.

TYPES: BEGIN OF ty_vbrk,
         vbeln  TYPE vbrk-vbeln,
         vkorg  TYPE vbrk-vkorg,
         fkart  TYPE vbrk-fkart,
         fkdat  TYPE vbrk-fkdat,
         refkey TYPE j_1bnflin-refkey,
         vbelv  TYPE vbfa-vbelv,
         netwr  TYPE  vbrk-netwr,
       END OF ty_vbrk,

       BEGIN OF ty_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         ntgew TYPE vbrp-ntgew,
         werks TYPE vbrp-werks,
         gsber TYPE vbrp-gsber,
         vkbur TYPE vbrp-vkbur,
         matnr TYPE vbrp-matnr,
         matkl TYPE vbrp-matkl,
         netwr TYPE  vbrp-netwr,
       END OF ty_vbrp,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         vbtyp_n TYPE vbfa-vbtyp_n,
       END OF ty_vbfa,

       BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,

       END OF ty_j_1bnflin,

       BEGIN OF ty_zfiwrt0001,
         cod_operacao TYPE zfiwrt0001-operacao,
       END OF ty_zfiwrt0001,

       BEGIN OF ty_j_1bnfe_active,
         docnum TYPE j_1bnfe_active-docnum,
       END OF ty_j_1bnfe_active,

       BEGIN OF ty_zfiwrt0009,
         matkl TYPE mara-matkl.
         INCLUDE STRUCTURE zfiwrt0009.
       TYPES  END OF ty_zfiwrt0009.




DATA: tg_0155           TYPE TABLE OF zsdt0155          WITH HEADER LINE,
      tg_0155_ger       TYPE TABLE OF zsdt0155          WITH HEADER LINE,
      tg_0156           TYPE TABLE OF zsdt0156          WITH HEADER LINE,
      tg_vbrk           TYPE TABLE OF ty_vbrk           WITH HEADER LINE,
      tg_vbrp           TYPE TABLE OF ty_vbrp           WITH HEADER LINE,
      tg_vbrk_est       TYPE TABLE OF ty_vbrk           WITH HEADER LINE,
      tg_j_1bnflin      TYPE TABLE OF ty_j_1bnflin      WITH HEADER LINE,
      tg_j_1bnfe_active TYPE TABLE OF ty_j_1bnfe_active WITH HEADER LINE,
      tg_vbfa_est       TYPE TABLE OF ty_vbfa           WITH HEADER LINE,
      tg_zfiwrt0008_est TYPE TABLE OF zfiwrt0008        WITH HEADER LINE,
      tg_zfiwrt0008     TYPE TABLE OF zfiwrt0008        WITH HEADER LINE,
      tg_zfiwrt0009     TYPE TABLE OF ty_zfiwrt0009     WITH HEADER LINE,
      tg_zfiwrt0001     TYPE TABLE OF ty_zfiwrt0001        WITH HEADER LINE.

DATA: vg_fkdat TYPE vbrk-fkdat,
      p_region TYPE adrc-region.

SELECTION-SCREEN BEGIN OF BLOCK a1.
SELECT-OPTIONS:
    p_vkorg  FOR vbrk-vkorg  NO INTERVALS NO-EXTENSION NO-DISPLAY,
    p_fkdat  FOR vbrk-fkdat NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END  OF BLOCK a1.

START-OF-SELECTION.

  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'ZSDR0081_JOB'
     AND status EQ 'R'.



  IF ( vg_job EQ 1 ) OR ( p_fkdat-low IS NOT INITIAL ).

    "Fluxo SD
    PERFORM: f_seleciona_dad0s_sd,
             f_processar_dados_sd.

    "Fluxo ZNFW
    PERFORM: f_seleciona_dados_znfw,
             f_processar_dados_znfw.

  ENDIF.

*FORM F_SELECIONA_DADOS .
*
*
*
*  PERFORM F_SELECIONA_DADOS_ZNFW.
*
*ENDFORM.

FORM f_check_filial USING p_bukrs  TYPE vbrk-vkorg
                          p_filial TYPE vbrp-werks
                 CHANGING p_ok     TYPE c
                          p_region TYPE adrc-region.

  CLEAR: p_ok.

  CHECK ( p_bukrs IS NOT INITIAL ) AND ( p_filial IS NOT INITIAL ).

  SELECT SINGLE *
    FROM j_1bbranch INTO @DATA(_wl_branch)
   WHERE bukrs  = @p_bukrs
     AND branch = @p_filial.

  CHECK sy-subrc = 0.

  "Endereco Filial
  SELECT SINGLE *
    FROM adrc INTO @DATA(_wl_adrc)
   WHERE addrnumber EQ @_wl_branch-adrnr.

  CHECK ( sy-subrc = 0 ) AND  ( _wl_branch-adrnr IS NOT INITIAL ). " AND ( _WL_ADRC-REGION EQ 'MT' OR _WL_ADRC-REGION EQ 'TO' ).

  p_ok = 'X'.
  p_region = _wl_adrc-region.

ENDFORM.


FORM f_processar_dados.

  PERFORM f_processar_dados_sd.
  PERFORM f_processar_dados_znfw.

ENDFORM.

FORM f_iniciar_variaveis .

  CLEAR:

  tg_0155[],
  tg_0156[],
  tg_vbrk[],
  tg_vbrp[],
  tg_j_1bnflin[],
  tg_j_1bnfe_active[],
  tg_vbfa_est[],
  tg_vbrk_est[],
  tg_zfiwrt0008_est,
  tg_zfiwrt0008,
  tg_zfiwrt0009,
  tg_zfiwrt0001.

  CLEAR: vg_fkdat.


ENDFORM.

FORM f_gerar_doc_ctb TABLES t_0155 STRUCTURE zsdt0155
                      USING p_vbrk TYPE vbrk
                            p_vbrp TYPE vbrp
                  CHANGING c_zib_gerada.

  DATA: wl_zib_contabil TYPE zib_contabil,
        it_zib_contabil TYPE TABLE OF zib_contabil,
        tg_0157         TYPE TABLE OF zsdt0157 WITH HEADER LINE,
        v_calc_1        TYPE p DECIMALS 10,
        v_dmbtr_ctb     TYPE bsis-dmbtr,
        wl_zsdt0156     TYPE zsdt0156,
        v_seqitem       TYPE zib_contabil-seqitem.

  DATA: v_ctb_seq TYPE zsdt0156-seq,
        DATA(10)  TYPE c,
        dia(2)    TYPE c,
        mes(2)    TYPE c,
        ano(4)    TYPE c.

  CLEAR: c_zib_gerada, v_calc_1, v_dmbtr_ctb, v_ctb_seq, tg_0157[], it_zib_contabil[].

  CHECK t_0155[] IS NOT INITIAL.

  "Pega Ultimo Seq Gerado
  SELECT *
    FROM zsdt0156 INTO TABLE @DATA(_tg_0156)
   WHERE vbeln   EQ @p_vbrp-vbeln
     AND posnr   EQ @p_vbrp-posnr.

  SORT: _tg_0156 BY vbeln posnr seq DESCENDING.
  LOOP AT _tg_0156 INTO DATA(_wl_056) WHERE vbeln   = p_vbrp-vbeln
                                        AND posnr   = p_vbrp-posnr.
    v_ctb_seq = _wl_056-seq.
    EXIT.
  ENDLOOP.

  ADD 1 TO v_ctb_seq.

  CONCATENATE p_vbrp-vbeln p_vbrp-posnr v_ctb_seq INTO DATA(v_obj_key).

  dia = p_vbrk-fkdat+6(2).
  mes = p_vbrk-fkdat+4(2).
  ano = p_vbrk-fkdat(4).

  CONCATENATE dia '.' mes '.' ano INTO data.

  SELECT SINGLE *
    FROM zib_contabil INTO @DATA(_wl_zib)
   WHERE obj_key EQ @v_obj_key.

  CHECK ( sy-subrc NE 0 ).

  v_seqitem = 1.

  LOOP AT t_0155 INTO DATA(p_0155).
    IF p_0155-calc_vlr_per IS INITIAL.

      CHECK ( p_0155-vlr_upf > 0 ) AND ( p_0155-perc_upf > 0 ) AND ( p_vbrp-ntgew > 0 ) .

      v_calc_1 =  ( p_0155-perc_upf * p_0155-vlr_upf ) / 100.

      CHECK v_calc_1 > 0.

      v_dmbtr_ctb = ( v_calc_1 * p_vbrp-ntgew ) / 1000.
    ELSE.
      v_dmbtr_ctb  = ( p_vbrp-netwr * p_0155-perc_upf ) / 100.
    ENDIF.



    CHECK  v_dmbtr_ctb > 0.

    DO 2 TIMES.

      CLEAR: wl_zib_contabil.

      wl_zib_contabil-obj_key         = v_obj_key.
      wl_zib_contabil-seqitem         = v_seqitem.

      CASE sy-index.
        WHEN 1.
          wl_zib_contabil-bschl       = '40'.
          wl_zib_contabil-hkont       = p_0155-hkont_deb.
        WHEN 2.
          wl_zib_contabil-bschl       = '50'.
          wl_zib_contabil-hkont       = p_0155-hkont_cred.
      ENDCASE.

      wl_zib_contabil-gsber           = p_vbrp-gsber.
      wl_zib_contabil-bukrs           = p_vbrk-vkorg.
      wl_zib_contabil-interface       = '00'.
      wl_zib_contabil-bktxt           = 'Contribuições Estaduais'.
      wl_zib_contabil-bldat           = data.
      wl_zib_contabil-budat           = data.
      wl_zib_contabil-gjahr           = ano.
      wl_zib_contabil-monat           = mes.
      wl_zib_contabil-blart           = 'RV'.
      wl_zib_contabil-matnr           = p_vbrp-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_vbrp-vbeln
        IMPORTING
          output = wl_zib_contabil-xblnr.

      "CONCATENATE 'Fatura' P_VBRP-VBELN INTO DATA(V_SGTXT) SEPARATED BY SPACE.

      wl_zib_contabil-waers          = 'BRL'.
      wl_zib_contabil-kidno          = space.
      "WL_ZIB_CONTABIL-SGTXT          = V_SGTXT.
      wl_zib_contabil-bupla          = p_vbrp-werks.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_vbrp-vbeln
        IMPORTING
          output = wl_zib_contabil-zuonr.

      wl_zib_contabil-waers_i        = 'BRL'.
      wl_zib_contabil-waers_f        = 'BRL'.
      wl_zib_contabil-wrbtr          = v_dmbtr_ctb.
      wl_zib_contabil-dmbtr          = v_dmbtr_ctb.
      "WL_ZIB_CONTABIL-DMBE2          = SPACE.
      wl_zib_contabil-rg_atualizado  = 'N'.

      APPEND wl_zib_contabil TO it_zib_contabil.
      ADD 1 TO v_seqitem.

    ENDDO.

    CLEAR: tg_0157.
    tg_0157-obj_key = v_obj_key.
    tg_0157-tp_trib = p_0155-tp_trib.
    tg_0157-dmbtr   = v_dmbtr_ctb.
    APPEND tg_0157.

  ENDLOOP.

  CHECK it_zib_contabil[] IS NOT INITIAL.
  CHECK tg_0157[] IS NOT INITIAL.

  INSERT zib_contabil FROM TABLE it_zib_contabil.

  IF ( sy-subrc NE 0 ).
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  INSERT zsdt0157 FROM TABLE tg_0157.
  IF ( sy-subrc NE 0 ).
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  CLEAR: wl_zsdt0156.
  wl_zsdt0156-vbeln       = p_vbrp-vbeln.
  wl_zsdt0156-posnr       = p_vbrp-posnr.
  wl_zsdt0156-seq         = v_ctb_seq.
  wl_zsdt0156-bukrs       = p_vbrk-vkorg.
  wl_zsdt0156-gjahr       = p_vbrk-fkdat(4).
  wl_zsdt0156-obj_key     = v_obj_key.
  wl_zsdt0156-dt_registro = sy-datum.
  wl_zsdt0156-hr_registro = sy-uzeit.
  APPEND wl_zsdt0156 TO tg_0156.

  MODIFY zsdt0156 FROM wl_zsdt0156.
  IF ( sy-subrc EQ 0 ).
    c_zib_gerada = 'X'.
  ENDIF.



ENDFORM.

FORM f_atualiza_doc_ctb USING p_data_ini TYPE erdat.

  DATA: wl_zib_chv TYPE zib_contabil_chv,
        wl_zib_err TYPE zib_contabil_err.

  CHECK p_data_ini IS NOT INITIAL.

  "Pega Ultimo Seq Gerado
  SELECT *
    FROM zsdt0156 INTO TABLE @DATA(_tg_0156)
   WHERE dt_registro >= @p_data_ini
     AND st_proc     EQ @space
     AND anulado     EQ @space.

  LOOP AT _tg_0156 INTO DATA(_wl_0156).

    PERFORM f_retorna_status_zib USING _wl_0156-obj_key
                              CHANGING wl_zib_chv
                                       wl_zib_err.

    DATA(_modify) = ''.
    IF ( wl_zib_chv IS NOT INITIAL ) AND ( wl_zib_chv-belnr IS NOT INITIAL ).
      _wl_0156-st_proc = '1'. "Processado com sucesso
      _wl_0156-belnr   = wl_zib_chv-belnr.
      _modify = 'X'.
    ELSEIF ( wl_zib_err IS NOT INITIAL ).
      _wl_0156-st_proc = '2'. "Erro no processamento
      _modify = 'X'.
    ENDIF.

    IF _modify = 'X'.
      MODIFY zsdt0156 FROM _wl_0156.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_retorna_status_zib USING p_objkey TYPE zib_contabil-obj_key
                       CHANGING p_zibchv TYPE zib_contabil_chv
                                p_ziberr TYPE zib_contabil_err.

  CLEAR: p_zibchv, p_ziberr.

  CHECK p_objkey IS NOT INITIAL.

  SELECT SINGLE *
    FROM zib_contabil_chv INTO p_zibchv
   WHERE obj_key = p_objkey.

  IF ( sy-subrc NE 0 ).
    SELECT SINGLE *
      FROM zib_contabil_err INTO p_ziberr
     WHERE obj_key = p_objkey.
  ENDIF.

ENDFORM.

*FORM F_PROC_ESTORNO.
*
*  PERFORM F_PROC_ESTORNO_SD.
*  PERFORM F_PROC_ESTORNO_NFW.
*
*  LOOP AT TG_VBRK_EST INTO DATA(WL_VBRK_EST).
*
*    LOOP AT TG_0156 WHERE VBELN   EQ WL_VBRK_EST-VBELV
*                      AND BELNR   IS NOT INITIAL
*                      AND STBLG   IS INITIAL     "Não foi estornado
*                      AND ANULADO IS INITIAL.    "Não foi anulado
*
*      UPDATE ZSDT0156 SET ESTORNADO = 'X'
*       WHERE OBJ_KEY = TG_0156-OBJ_KEY.
*
*      CHECK SY-SUBRC = 0.
*
*      PERFORM F_ESTORNO_CTB USING TG_0156
*                                  WL_VBRK_EST.
*
*    ENDLOOP.
*
*  ENDLOOP.
*
*ENDFORM.

*FORM F_ESTORNO_CTB USING P_ZSDT0156 TYPE ZSDT0156
*                         P_VBRK_EST TYPE TY_VBRK.
*
*
*
*PERFORM F_ESTORNO_CTB_SD USING P_ZSDT0156 TYPE ZSDT0156
*                         P_VBRK_EST TYPE TY_VBRK.
*PERFORM F_ESTORNO_CTB_NFW USING P_ZSDT0156 TYPE ZSDT0156
*                         P_VBRK_EST TYPE TY_VBRK.
*
*
*  DATA: IT_DTA   TYPE STANDARD TABLE OF BDCDATA,
*        WA_DTA   TYPE BDCDATA,
*        WG_BDC   TYPE BDCDATA,
*        TG_BDC   TYPE TABLE OF BDCDATA,
*        TG_MSG   TYPE TABLE OF BDCMSGCOLL,
*        WG_MSG   TYPE BDCMSGCOLL,
*        OPT      TYPE CTU_PARAMS,
*        VL_STBLG TYPE BKPF-STBLG,
*        VL_DATA  TYPE DATS.
*
*  CHECK ( P_ZSDT0156-BELNR IS NOT INITIAL ) AND
*        ( P_VBRK_EST-FKDAT IS NOT INITIAL ).
*
*  CONCATENATE P_VBRK_EST-FKDAT+6(2)   P_VBRK_EST-FKDAT+4(2) P_VBRK_EST-FKDAT(4) INTO VL_DATA.
*
*  FREE: IT_DTA.
*  DEFINE SHDB.
*    CLEAR WA_DTA.
*    WA_DTA-PROGRAM   = &1.
*    WA_DTA-DYNPRO    = &2.
*    WA_DTA-DYNBEGIN  = &3.
*    WA_DTA-FNAM      = &4.
*    WA_DTA-FVAL      = &5.
*    APPEND WA_DTA TO IT_DTA.
*  END-OF-DEFINITION.
*
*  SHDB:
*  'SAPMF05A' '0105' 'X'  ' '           ' ',
*  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
*  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
*  ' '        ' '    ' '  'RF05A-BELNS' P_ZSDT0156-BELNR ,
*  ' '        ' '    ' '  'BKPF-BUKRS'  P_ZSDT0156-BUKRS,
*  ' '        ' '    ' '  'RF05A-GJAHS' P_ZSDT0156-GJAHR,
*  ' '        ' '    ' '  'UF05A-STGRD' '02',
*  ' '        ' '    ' '  'BSIS-BUDAT'  VL_DATA.
*
*  OPT-DISMODE = 'N'.
*  CALL TRANSACTION 'FB08' USING IT_DTA OPTIONS FROM OPT.
*
*  CHECK SY-SUBRC IS INITIAL.
*
*  SELECT SINGLE STBLG
*    FROM BKPF INTO VL_STBLG
*   WHERE BUKRS = P_ZSDT0156-BUKRS
*     AND BELNR = P_ZSDT0156-BELNR
*     AND GJAHR = P_ZSDT0156-GJAHR.
*
*  CHECK ( SY-SUBRC = 0 ) AND ( VL_STBLG IS NOT INITIAL ).
*
*  UPDATE ZSDT0156 SET STBLG = VL_STBLG
*   WHERE OBJ_KEY = P_ZSDT0156-OBJ_KEY.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DAD0S_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dad0s_sd .

  DATA: v_data_prc_estorno TYPE erdat.

  PERFORM: f_iniciar_variaveis.

  vg_fkdat = p_fkdat-low.

  IF vg_fkdat IS INITIAL.
    vg_fkdat = sy-datum - 1.
  ENDIF.

  CHECK vg_fkdat IS NOT INITIAL.

  "Carrega Faturas Estornadas
  v_data_prc_estorno = vg_fkdat - 1.

  SELECT vb~vbelv ftn~fkdat INTO CORRESPONDING FIELDS OF TABLE tg_vbrk_est
    FROM vbfa AS vb INNER JOIN vbrk AS ftn ON vb~vbeln  = ftn~vbeln
   WHERE vb~vbtyp_n = 'N'
     AND ftn~erdat  >=  v_data_prc_estorno
     AND EXISTS ( SELECT *
                    FROM zsdt0156 AS c
                   WHERE c~vbeln = vb~vbelv ).

  IF tg_vbrk_est[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0156 APPENDING TABLE tg_0156
       FOR ALL ENTRIES IN tg_vbrk_est
     WHERE vbeln  EQ tg_vbrk_est-vbelv.
  ENDIF.

  "Parâmetros Válidos na data atual
  SELECT *
    FROM zsdt0155 INTO TABLE tg_0155
   WHERE dt_ini    <= sy-datum
     AND dt_fim    >= sy-datum
     AND cancelado = ''.

  "Parâmetros Válidos na data atual
  SELECT *
  FROM zsdt0155 APPENDING TABLE tg_0155
   WHERE dt_ini    <= vg_fkdat
     AND dt_fim    >= vg_fkdat
     AND cancelado = ''.

  DELETE tg_0155 WHERE ( bukrs       IS INITIAL ) OR
                       ( auart       IS INITIAL ) OR
                       ( tp_trib     IS INITIAL ) OR
                       ( hkont_deb   IS INITIAL ) OR
                       ( hkont_cred  IS INITIAL ) OR
                       ( matnr       IS INITIAL AND
                         matkl       IS INITIAL ).

  SORT tg_0155 BY bukrs auart tp_trib matkl matnr dt_ini DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_0155 COMPARING bukrs auart tp_trib matkl matnr.

  CHECK tg_0155[] IS NOT INITIAL.

  "Cabeçalho Faturas
  SELECT *
    FROM vbrk APPENDING CORRESPONDING FIELDS OF TABLE tg_vbrk
     FOR ALL ENTRIES IN tg_0155
   WHERE bukrs   EQ tg_0155-bukrs
     AND fkart   EQ tg_0155-auart
     AND fkdat   EQ vg_fkdat AND DRAFT = SPACE .

  CHECK tg_vbrk[] IS NOT INITIAL.

  LOOP AT tg_vbrk.
    tg_vbrk-refkey = tg_vbrk-vbeln.
    MODIFY tg_vbrk.
  ENDLOOP.

  "Itens Doc. Fiscal
  SELECT *
    FROM j_1bnflin APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnflin
     FOR ALL ENTRIES IN tg_vbrk
   WHERE refkey EQ tg_vbrk-refkey.

  IF tg_j_1bnflin[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
       FOR ALL ENTRIES IN tg_j_1bnflin
     WHERE docnum     = tg_j_1bnflin-docnum
       AND cancel     = ''
       AND docsta     = '1'.
  ENDIF.

  "Itens Faturas
  SELECT *
    FROM vbrp APPENDING CORRESPONDING FIELDS OF TABLE tg_vbrp
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbeln EQ tg_vbrk-vbeln.

  "Fluxo de Documentos Estornados
  SELECT *
    FROM vbfa APPENDING CORRESPONDING FIELDS OF TABLE tg_vbfa_est
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbelv   EQ tg_vbrk-vbeln
     AND vbtyp_n EQ 'N'. "Estorno Fatura

  "Documentos já Gerados
  SELECT *
    FROM zsdt0156 APPENDING TABLE tg_0156
     FOR ALL ENTRIES IN tg_vbrk
   WHERE vbeln  EQ tg_vbrk-vbeln.


  SORT tg_vbrk BY vbeln.
  DELETE ADJACENT DUPLICATES FROM tg_vbrk COMPARING vbeln.

  SORT tg_vbrp BY vbeln posnr.
  DELETE ADJACENT DUPLICATES FROM tg_vbrp COMPARING vbeln posnr.

  SORT tg_0156 BY vbeln posnr seq.
  DELETE ADJACENT DUPLICATES FROM tg_0156 COMPARING vbeln posnr seq.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS_ZNFW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados_znfw .

  DATA: v_data_prc_estorno TYPE erdat.

  PERFORM: f_iniciar_variaveis.

  vg_fkdat = p_fkdat-low.

  IF vg_fkdat IS INITIAL.
    vg_fkdat = sy-datum - 1.
  ENDIF.

  CHECK vg_fkdat IS NOT INITIAL.

  "Carrega Faturas Estornadas
  v_data_prc_estorno = vg_fkdat - 1.


  SELECT vb~seq_lcto vb~dt_estorno INTO CORRESPONDING FIELDS OF TABLE tg_zfiwrt0008_est
    FROM zfiwrt0008 AS vb
   WHERE vb~docs_estornados = 'X'
     AND vb~dt_estorno  >=  v_data_prc_estorno
     AND EXISTS ( SELECT *
                    FROM zsdt0156 AS c
                   WHERE c~seq_lcto = vb~seq_lcto ).

  IF tg_zfiwrt0008_est[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0156 APPENDING TABLE tg_0156
       FOR ALL ENTRIES IN tg_zfiwrt0008_est
     WHERE seq_lcto  EQ tg_zfiwrt0008_est-seq_lcto.
  ENDIF.

  "Parâmetros Válidos na data atual
  SELECT *
    FROM zsdt0155 INTO TABLE tg_0155
   WHERE dt_ini    <= sy-datum
     AND dt_fim    >= sy-datum
     AND cancelado = ''.

  "Parâmetros Válidos na data atual
  SELECT *
    FROM zsdt0155 APPENDING TABLE tg_0155
   WHERE dt_ini    <= vg_fkdat
     AND dt_fim    >= vg_fkdat
     AND cancelado = ''.

  DELETE tg_0155 WHERE ( bukrs       IS INITIAL ) OR
                       ( lanc_znfw   IS INITIAL ) OR
                       ( tp_trib     IS INITIAL ) OR
                       ( hkont_deb   IS INITIAL ) OR
                       ( hkont_cred  IS INITIAL ) OR
                       ( matnr       IS INITIAL AND
                         matkl       IS INITIAL ).

  SORT tg_0155 BY bukrs lanc_znfw tp_trib matkl matnr dt_ini DESCENDING.
  DELETE ADJACENT DUPLICATES FROM tg_0155 COMPARING bukrs lanc_znfw tp_trib matkl matnr.

  CHECK tg_0155[] IS NOT INITIAL.

  "Cabeçalho Faturas
  SELECT *
    FROM zfiwrt0008 APPENDING CORRESPONDING FIELDS OF TABLE tg_zfiwrt0008
   WHERE  dt_ult_mod  EQ vg_fkdat.

  DELETE tg_zfiwrt0008 WHERE ( docs_estornados = 'X' ) OR
                             ( docnum IS INITIAL ).

  CHECK tg_zfiwrt0008[] IS NOT INITIAL.

  SELECT operacao
      FROM zfiwrt0001
      INTO TABLE tg_zfiwrt0001
    FOR ALL ENTRIES IN tg_zfiwrt0008
      WHERE operacao = tg_zfiwrt0008-operacao
    AND lm_contri_uf = 'S'.

  LOOP AT tg_zfiwrt0008.

    READ TABLE tg_zfiwrt0001 WITH KEY cod_operacao = tg_zfiwrt0008-operacao.

    IF sy-subrc NE 0.
      DELETE tg_zfiwrt0008.
    ENDIF.


  ENDLOOP.

  CHECK tg_zfiwrt0008[] IS NOT INITIAL.

  "Itens Doc. Fiscal
  SELECT *
    FROM j_1bnflin APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnflin
     FOR ALL ENTRIES IN tg_zfiwrt0008
   WHERE docnum EQ tg_zfiwrt0008-docnum.

  CHECK tg_j_1bnflin[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfe_active
     FOR ALL ENTRIES IN tg_j_1bnflin
   WHERE docnum     = tg_j_1bnflin-docnum
     AND cancel     = ''
     AND docsta     = '1'.


  "Itens Faturas
  SELECT *
    FROM  zfiwrt0009 APPENDING CORRESPONDING FIELDS OF TABLE tg_zfiwrt0009
     FOR ALL ENTRIES IN tg_zfiwrt0008
   WHERE seq_lcto EQ tg_zfiwrt0008-seq_lcto.

  LOOP AT tg_zfiwrt0009 ASSIGNING FIELD-SYMBOL(<wa_zfiwrt0009>).

    SELECT SINGLE matkl
  FROM mara
  INTO @DATA(matkl)
  WHERE matnr = @<wa_zfiwrt0009>-matnr.

    IF sy-subrc EQ 0.
      <wa_zfiwrt0009>-matkl = matkl.
    ENDIF.
  ENDLOOP.


  "Documentos já Gerados
  SELECT *
    FROM zsdt0156 APPENDING TABLE tg_0156
     FOR ALL ENTRIES IN tg_zfiwrt0008
   WHERE seq_lcto  EQ tg_zfiwrt0008-seq_lcto.

  SORT tg_zfiwrt0008 BY seq_lcto.
  DELETE ADJACENT DUPLICATES FROM  tg_zfiwrt0008 COMPARING seq_lcto .

  SORT tg_zfiwrt0009 BY seq_lcto itmnum.
  DELETE ADJACENT DUPLICATES FROM tg_zfiwrt0009 COMPARING seq_lcto .

  SORT tg_0156 BY vbeln posnr seq.
  DELETE ADJACENT DUPLICATES FROM tg_0156 COMPARING seq_lcto itmnum seq.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAR_DADOS_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processar_dados_sd .

  DATA: v_ctb_ger     TYPE c,
        wl_vbrk       TYPE vbrk,
        wl_vbrp       TYPE vbrp,
        v_dt_atualiza TYPE erdat.

  LOOP AT tg_vbrk. " Fatura Cabeçalho

    READ TABLE tg_vbfa_est WITH KEY vbelv = tg_vbrk-vbeln. "Check se fatura foi estornada
    CHECK sy-subrc NE 0.

    READ TABLE tg_j_1bnflin WITH KEY refkey = tg_vbrk-refkey.
    CHECK sy-subrc = 0.

    READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnflin-docnum.
    CHECK sy-subrc = 0.

    LOOP AT tg_vbrp WHERE vbeln EQ tg_vbrk-vbeln. " Fatura Itens

      DATA(_ok) = ''.
      PERFORM f_check_filial USING tg_vbrk-vkorg
                                   tg_vbrp-werks
                          CHANGING _ok
                                   p_region.
      CHECK _ok IS NOT INITIAL.


      v_ctb_ger  = ''.
      LOOP AT tg_0156 WHERE vbeln   EQ tg_vbrp-vbeln
                        AND posnr   EQ tg_vbrp-posnr
                        AND stblg   IS INITIAL   "Não foi estornado
                        AND anulado IS INITIAL.  "Não foi anulado
        v_ctb_ger = 'X'.
        EXIT.
      ENDLOOP.

      IF v_ctb_ger IS INITIAL. "Caso não tenha gerado Doc. Contábil

        CLEAR: wl_vbrk, wl_vbrp, tg_0155_ger[].


**--USER STORY 83809 / Anderson Oenning { Incluido a condição dt_ini e dt_fim na codição da seleção }.
        "Carregar Parâmetros
        LOOP AT tg_0155 WHERE bukrs  EQ tg_vbrk-vkorg
                          AND auart  EQ tg_vbrk-fkart
                          AND region EQ p_region
                          AND dt_ini <= tg_vbrk-fkdat
                          AND dt_fim >= tg_vbrk-fkdat
                          AND ( matnr EQ tg_vbrp-matnr  OR
                                matkl EQ tg_vbrp-matkl ).
          APPEND tg_0155 TO tg_0155_ger.
        ENDLOOP.
**--USER STORY 83809 / Anderson Oenning { Incluido a condição dt_ini e dt_fim na codição da seleção }.

        CHECK tg_0155_ger[] IS NOT INITIAL.

        MOVE-CORRESPONDING: tg_vbrk TO wl_vbrk,
                            tg_vbrp TO wl_vbrp.

        DATA(_zib_ger) = ''.
        PERFORM f_gerar_doc_ctb TABLES tg_0155_ger
                                 USING wl_vbrk
                                       wl_vbrp
                             CHANGING _zib_ger.
      ENDIF.

    ENDLOOP.

  ENDLOOP.



  "Atualizar Documentos Contabeis
  v_dt_atualiza = sy-datum - 2.
  PERFORM f_atualiza_doc_ctb USING v_dt_atualiza.

  "Processar estornos
  PERFORM f_proc_estorno_sd.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAR_DADOS_ZNFW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processar_dados_znfw .

  DATA: v_ctb_ger     TYPE c,
        wl_zfiwrt0008 TYPE zfiwrt0008,
        wl_zfiwrt0009 TYPE zfiwrt0009,
        v_dt_atualiza TYPE erdat.

  LOOP AT tg_zfiwrt0008. " Fatura Cabeçalho

    READ TABLE tg_zfiwrt0008_est WITH KEY seq_lcto = tg_zfiwrt0008-seq_lcto. "Check se fatura foi estornada
    CHECK sy-subrc NE 0.

    READ TABLE tg_j_1bnflin WITH KEY docnum = tg_zfiwrt0008-docnum. "ANALISAR    sei que ta errado
    CHECK sy-subrc = 0.

    READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnflin-docnum.
    CHECK sy-subrc = 0.

    LOOP AT tg_zfiwrt0009 WHERE seq_lcto EQ tg_zfiwrt0008-seq_lcto. " Fatura Itens

      DATA(_ok) = ''.
      PERFORM f_check_filial USING tg_zfiwrt0008-bukrs
                                   tg_zfiwrt0008-branch
                          CHANGING _ok
                                   p_region.
      CHECK _ok IS NOT INITIAL.


      v_ctb_ger  = ''.
      LOOP AT tg_0156 WHERE seq_lcto   EQ tg_zfiwrt0009-seq_lcto
                        AND itmnum   EQ tg_zfiwrt0009-itmnum
                        AND stblg   IS INITIAL   "Não foi estornado
                        AND anulado IS INITIAL.  "Não foi anulado
        v_ctb_ger = 'X'.
        EXIT.
      ENDLOOP.

      IF v_ctb_ger IS INITIAL. "Caso não tenha gerado Doc. Contábil

        CLEAR: wl_zfiwrt0008, wl_zfiwrt0009, tg_0155_ger[].

**--USER STORY 83809 / Anderson Oenning { Incluido a condição dt_ini e dt_fim na codição da seleção }.
        "Carregar Parâmetros
        LOOP AT tg_0155 WHERE bukrs     EQ tg_zfiwrt0008-bukrs
                          AND lanc_znfw EQ abap_true
                          AND region    EQ p_region
                          AND dt_ini    <= tg_zfiwrt0008-budat
                          AND dt_fim    >= tg_zfiwrt0008-budat
                          AND ( matnr   EQ tg_zfiwrt0009-matnr  OR
                                matkl   EQ tg_zfiwrt0009-matkl ).
          APPEND tg_0155 TO tg_0155_ger.
        ENDLOOP.
**--USER STORY 83809 / Anderson Oenning { Incluido a condição dt_ini e dt_fim na codição da seleção }.
        CHECK tg_0155_ger[] IS NOT INITIAL.

        MOVE-CORRESPONDING: tg_zfiwrt0008 TO wl_zfiwrt0008,
                            tg_zfiwrt0009 TO wl_zfiwrt0009.

        DATA(_zib_ger) = ''.
        PERFORM f_gerar_doc_ctb_znfw TABLES tg_0155_ger
                                 USING wl_zfiwrt0008
                                       wl_zfiwrt0009
                             CHANGING _zib_ger.
      ENDIF.

    ENDLOOP.

  ENDLOOP.


  "Atualizar Documentos Contabeis
  v_dt_atualiza = sy-datum - 2.
  PERFORM f_atualiza_doc_ctb USING v_dt_atualiza.

  "Processar estornos
  PERFORM f_proc_estorno_nfw.

ENDFORM.



FORM f_gerar_doc_ctb_znfw TABLES t_0155 STRUCTURE zsdt0155
                      USING p_zfiwrt0008 TYPE zfiwrt0008
                            p_zfiwrt0009 TYPE zfiwrt0009
                  CHANGING c_zib_gerada.

  DATA: wl_zib_contabil TYPE zib_contabil,
        it_zib_contabil TYPE TABLE OF zib_contabil,
        tg_0157         TYPE TABLE OF zsdt0157 WITH HEADER LINE,
        v_calc_1        TYPE p DECIMALS 10,
        v_dmbtr_ctb     TYPE bsis-dmbtr,
        wl_zsdt0156     TYPE zsdt0156,
        v_seqitem       TYPE zib_contabil-seqitem.

  DATA: v_ctb_seq TYPE zsdt0156-seq,
        DATA(10)  TYPE c,
        dia(2)    TYPE c,
        mes(2)    TYPE c,
        ano(4)    TYPE c,
        v_menge   TYPE zfiwrt0009-menge.

  CLEAR: c_zib_gerada, v_calc_1, v_dmbtr_ctb, v_ctb_seq, tg_0157[], it_zib_contabil[].

  CHECK t_0155[] IS NOT INITIAL.

  "Pega Ultimo Seq Gerado
  SELECT *
    FROM zsdt0156 INTO TABLE @DATA(_tg_0156)
   WHERE seq_lcto   EQ @p_zfiwrt0009-seq_lcto
     AND itmnum   EQ @p_zfiwrt0009-itmnum.

  SORT: _tg_0156 BY seq_lcto itmnum seq DESCENDING.
  LOOP AT _tg_0156 INTO DATA(_wl_056) WHERE seq_lcto   = p_zfiwrt0009-seq_lcto
                                        AND itmnum   = p_zfiwrt0009-itmnum.
    v_ctb_seq = _wl_056-seq.
    EXIT.
  ENDLOOP.

  ADD 1 TO v_ctb_seq.

  CONCATENATE 'TB' p_zfiwrt0009-seq_lcto p_zfiwrt0009-itmnum v_ctb_seq INTO DATA(v_obj_key).

  dia = p_zfiwrt0008-budat+6(2).
  mes = p_zfiwrt0008-budat+4(2).
  ano = p_zfiwrt0008-budat(4).

  CONCATENATE dia '.' mes '.' ano INTO data.

  SELECT SINGLE *
    FROM zib_contabil INTO @DATA(_wl_zib)
   WHERE obj_key EQ @v_obj_key.

  CHECK ( sy-subrc NE 0 ).

  v_seqitem = 1.


  LOOP AT t_0155 INTO DATA(p_0155).

    CASE p_zfiwrt0009-meins.
      WHEN 'KG'.
        v_menge = p_zfiwrt0009-menge.
      WHEN 'TO'.
        v_menge = p_zfiwrt0009-menge * 1000.
      WHEN OTHERS .
        CONTINUE.
    ENDCASE.

    IF  p_0155-calc_vlr_per IS INITIAL.
      CHECK ( p_0155-vlr_upf > 0 ) AND ( p_0155-perc_upf > 0 ) AND ( v_menge > 0 ) .

      v_calc_1 =  ( p_0155-perc_upf * p_0155-vlr_upf ) / 100.

      CHECK v_calc_1 > 0.

      v_dmbtr_ctb = ( v_calc_1 * v_menge ) / 1000.
    ELSE.
      v_dmbtr_ctb  = ( p_zfiwrt0009-netwr * p_0155-perc_upf ) / 100.

    ENDIF.



    CHECK  v_dmbtr_ctb > 0.



    DO 2 TIMES.

      CLEAR: wl_zib_contabil.

      wl_zib_contabil-obj_key         = v_obj_key.
      wl_zib_contabil-seqitem         = v_seqitem.

      CASE sy-index.
        WHEN 1.
          wl_zib_contabil-bschl       = '40'.
          wl_zib_contabil-hkont       = p_0155-hkont_deb.
        WHEN 2.
          wl_zib_contabil-bschl       = '50'.
          wl_zib_contabil-hkont       = p_0155-hkont_cred.
      ENDCASE.
      "ANALISAR se é branch e bukrs mesmo
      wl_zib_contabil-gsber           = p_zfiwrt0008-branch.
      wl_zib_contabil-bukrs           = p_zfiwrt0008-bukrs.
      wl_zib_contabil-interface       = '00'.
      wl_zib_contabil-bktxt           = 'Contribuições Estaduais'.
      wl_zib_contabil-bldat           = data.
      wl_zib_contabil-budat           = data.
      wl_zib_contabil-gjahr           = ano.
      wl_zib_contabil-monat           = mes.
      wl_zib_contabil-blart           = 'RV'.
      wl_zib_contabil-matnr           = p_zfiwrt0009-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_zfiwrt0009-seq_lcto
        IMPORTING
          output = wl_zib_contabil-xblnr.

      "CONCATENATE 'Fatura' P_VBRP-VBELN INTO DATA(V_SGTXT) SEPARATED BY SPACE.

      wl_zib_contabil-waers          = 'BRL'.
      wl_zib_contabil-kidno          = space.
      "WL_ZIB_CONTABIL-SGTXT          = V_SGTXT. "ANALISAR pois parece estar errado essa linha
      wl_zib_contabil-bupla          = p_zfiwrt0008-branch.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_zfiwrt0009-seq_lcto
        IMPORTING
          output = wl_zib_contabil-zuonr.

      wl_zib_contabil-waers_i        = 'BRL'.
      wl_zib_contabil-waers_f        = 'BRL'.
      wl_zib_contabil-wrbtr          = v_dmbtr_ctb.
      wl_zib_contabil-dmbtr          = v_dmbtr_ctb.
      "WL_ZIB_CONTABIL-DMBE2          = SPACE.
      wl_zib_contabil-rg_atualizado  = 'N'.

      APPEND wl_zib_contabil TO it_zib_contabil.
      ADD 1 TO v_seqitem.

    ENDDO.

    CLEAR: tg_0157.
    tg_0157-obj_key = v_obj_key.
    tg_0157-tp_trib = p_0155-tp_trib.
    tg_0157-dmbtr   = v_dmbtr_ctb.
    APPEND tg_0157.

  ENDLOOP.

  CHECK it_zib_contabil[] IS NOT INITIAL.
  CHECK tg_0157[] IS NOT INITIAL.

  INSERT zib_contabil FROM TABLE it_zib_contabil.

  IF ( sy-subrc NE 0 ).
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  INSERT zsdt0157 FROM TABLE tg_0157.
  IF ( sy-subrc NE 0 ).
    ROLLBACK WORK.
    RETURN.
  ENDIF.

  CLEAR: wl_zsdt0156.
  wl_zsdt0156-seq_lcto     = p_zfiwrt0009-seq_lcto.
  wl_zsdt0156-itmnum       = p_zfiwrt0009-itmnum.
  wl_zsdt0156-seq         = v_ctb_seq.
  wl_zsdt0156-bukrs       = p_zfiwrt0008-bukrs.
  wl_zsdt0156-gjahr       = ano.
  wl_zsdt0156-obj_key     = v_obj_key.
  wl_zsdt0156-dt_registro = sy-datum.
  wl_zsdt0156-hr_registro = sy-uzeit.
  APPEND wl_zsdt0156 TO tg_0156.

  MODIFY zsdt0156 FROM wl_zsdt0156.
  IF ( sy-subrc EQ 0 ).
    c_zib_gerada = 'X'.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROC_ESTORNO_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_proc_estorno_sd .

  LOOP AT tg_vbrk_est INTO DATA(wl_vbrk_est).

    LOOP AT tg_0156 WHERE vbeln   EQ wl_vbrk_est-vbelv
                      AND belnr   IS NOT INITIAL
                      AND stblg   IS INITIAL     "Não foi estornado
                      AND anulado IS INITIAL.    "Não foi anulado

      UPDATE zsdt0156 SET estornado = 'X'
       WHERE obj_key = tg_0156-obj_key.

      CHECK sy-subrc = 0.

      PERFORM f_estorno_ctb_sd USING tg_0156
                                  wl_vbrk_est.

    ENDLOOP.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROC_ESTORNO_NFW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_proc_estorno_nfw .

  LOOP AT tg_zfiwrt0008_est INTO DATA(wl_zfiwrt0008_est).

    LOOP AT tg_0156 WHERE seq_lcto   EQ wl_zfiwrt0008_est-seq_lcto
                      AND belnr   IS NOT INITIAL
                      AND stblg   IS INITIAL     "Não foi estornado
                      AND anulado IS INITIAL.    "Não foi anulado

      UPDATE zsdt0156 SET estornado = 'X'
       WHERE obj_key = tg_0156-obj_key.

      CHECK sy-subrc = 0.

      PERFORM f_estorno_ctb_nfw USING tg_0156
                                  wl_zfiwrt0008_est.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTB_SD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_ctb_sd USING p_zsdt0156 TYPE zsdt0156
                         p_vbrk_est TYPE ty_vbrk .


  DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
        wa_dta   TYPE bdcdata,
        wg_bdc   TYPE bdcdata,
        tg_bdc   TYPE TABLE OF bdcdata,
        tg_msg   TYPE TABLE OF bdcmsgcoll,
        wg_msg   TYPE bdcmsgcoll,
        opt      TYPE ctu_params,
        vl_stblg TYPE bkpf-stblg,
        vl_data  TYPE dats.

  CHECK ( p_zsdt0156-belnr IS NOT INITIAL ) AND
        ( p_vbrk_est-fkdat IS NOT INITIAL ).

  CONCATENATE p_vbrk_est-fkdat+6(2)   p_vbrk_est-fkdat+4(2) p_vbrk_est-fkdat(4) INTO vl_data.

  FREE: it_dta.
  DEFINE shdb.
    CLEAR WA_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' p_zsdt0156-belnr ,
  ' '        ' '    ' '  'BKPF-BUKRS'  p_zsdt0156-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' p_zsdt0156-gjahr,
  ' '        ' '    ' '  'UF05A-STGRD' '02',
  ' '        ' '    ' '  'BSIS-BUDAT'  vl_data.

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE stblg
    FROM bkpf INTO vl_stblg
   WHERE bukrs = p_zsdt0156-bukrs
     AND belnr = p_zsdt0156-belnr
     AND gjahr = p_zsdt0156-gjahr.

  CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

  UPDATE zsdt0156 SET stblg = vl_stblg
   WHERE obj_key = p_zsdt0156-obj_key.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNO_CTB_NFW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorno_ctb_nfw  USING p_zsdt0156 TYPE zsdt0156
                         p_zfiwrt0008_est TYPE zfiwrt0008.


  DATA: it_dta   TYPE STANDARD TABLE OF bdcdata,
        wa_dta   TYPE bdcdata,
        wg_bdc   TYPE bdcdata,
        tg_bdc   TYPE TABLE OF bdcdata,
        tg_msg   TYPE TABLE OF bdcmsgcoll,
        wg_msg   TYPE bdcmsgcoll,
        opt      TYPE ctu_params,
        vl_stblg TYPE bkpf-stblg,
        vl_data  TYPE dats.

  CHECK ( p_zsdt0156-belnr IS NOT INITIAL ) AND
        ( p_zfiwrt0008_est-dt_estorno IS NOT INITIAL ). "ANALISAR  acho que essa data ta errada

  CONCATENATE p_zfiwrt0008_est-dt_estorno+6(2)   p_zfiwrt0008_est-dt_estorno+4(2) p_zfiwrt0008_est-dt_estorno(4) INTO vl_data. "ANALISAR  acho que essa data ta errada

  FREE: it_dta.
  DEFINE shdb.
    CLEAR WA_DTA.
    WA_DTA-PROGRAM   = &1.
    WA_DTA-DYNPRO    = &2.
    WA_DTA-DYNBEGIN  = &3.
    WA_DTA-FNAM      = &4.
    WA_DTA-FVAL      = &5.
    APPEND WA_DTA TO IT_DTA.
  END-OF-DEFINITION.

  shdb:
  'SAPMF05A' '0105' 'X'  ' '           ' ',
  ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
  ' '        ' '    ' '  'BDC_OKCODE'  '=BU',
  ' '        ' '    ' '  'RF05A-BELNS' p_zsdt0156-belnr ,
  ' '        ' '    ' '  'BKPF-BUKRS'  p_zsdt0156-bukrs,
  ' '        ' '    ' '  'RF05A-GJAHS' p_zsdt0156-gjahr,
  ' '        ' '    ' '  'UF05A-STGRD' '02',
  ' '        ' '    ' '  'BSIS-BUDAT'  vl_data. "ANALISAR   TA ERRADOOOOOO QUAIS CAMPOS EU TENHO QUE PREENCHER NO SHDB

  opt-dismode = 'N'.
  CALL TRANSACTION 'FB08' USING it_dta OPTIONS FROM opt.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE stblg
    FROM bkpf INTO vl_stblg
   WHERE bukrs = p_zsdt0156-bukrs
     AND belnr = p_zsdt0156-belnr
     AND gjahr = p_zsdt0156-gjahr.

  CHECK ( sy-subrc = 0 ) AND ( vl_stblg IS NOT INITIAL ).

  UPDATE zsdt0156 SET stblg = vl_stblg
   WHERE obj_key = p_zsdt0156-obj_key.

ENDFORM.
