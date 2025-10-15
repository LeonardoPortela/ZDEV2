
*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZMMI0002                                                *
* Descrição  : Importação de Dados Classificação HVI                   *
* Módulo     : MM                                Transação: ZMM0024    *
*                                                                      *
REPORT zmmi0002 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: t001w, mara, ztsafrafardos, zpp_kuhlmann_hvi,
        zppt0002.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_data,
         campo TYPE char1024,
       END   OF type_data,

       BEGIN OF type_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         objek TYPE inob-objek,
       END   OF type_mchb,

       BEGIN OF type_rmclm,
         klart TYPE tcla-klart,
         cuobj TYPE inob-cuobj,
         clint TYPE kssk-clint,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END   OF type_rmclm,

       BEGIN OF type_inob,
         cuobj  TYPE inob-cuobj,
         klart  TYPE inob-klart,
         obtab  TYPE inob-obtab,
         objek  TYPE inob-objek,
         objekk TYPE kssk-objek,
       END   OF type_inob,

       BEGIN OF type_kssk,
         objek TYPE kssk-objek,
         mafid TYPE kssk-mafid,
         klart TYPE kssk-klart,
         clint TYPE kssk-clint,
         adzhl TYPE kssk-adzhl,
       END   OF type_kssk,

       BEGIN OF type_klah,
         clint TYPE klah-clint,
         klart TYPE klah-klart,
         class TYPE klah-class,
         vondt TYPE klah-vondt,
         bisdt TYPE klah-bisdt,
       END   OF type_klah,

       BEGIN OF type_msg,
         material TYPE mchb-matnr,
         lote     TYPE mchb-charg,
         centro   TYPE mchb-werks,
         tipo     TYPE char1,
         numero   TYPE ztfardosmsg-numero,
         mensagem TYPE char40,
       END   OF type_msg,

       BEGIN OF type_ksml,
         clint TYPE ksml-clint,
         posnr TYPE ksml-posnr,
         adzhl TYPE ksml-adzhl,
         imerk TYPE ksml-imerk,
         klart TYPE ksml-klart,
       END   OF type_ksml,

       BEGIN OF type_cabnt,
         atinn TYPE cabnt-atinn,
         spras TYPE cabnt-spras,
         adzhl TYPE cabnt-adzhl,
         atbez TYPE cabnt-atbez,
         atnam TYPE cabn-atnam,
         atfor TYPE cabn-atfor,
       END   OF type_cabnt.
*
*
*** Criação de tabela dinamica
*DATA: T_FIELDCATALOG TYPE LVC_T_FCAT,
*      T_FIELDCAT_EDT TYPE LVC_T_FCAT,
*      W_FIELDCATALOG TYPE LVC_S_FCAT,
*      T_NEW_TABLE    TYPE REF TO DATA,
*      T_NEW_LINE     TYPE REF TO DATA,
*      WA_LAYOUT      TYPE LVC_S_LAYO,
*      WA_STABLE      TYPE LVC_S_STBL.

TYPES: BEGIN OF ty_zmme_cl.
         INCLUDE TYPE zmme_cl.
         TYPES: check TYPE c,
       END OF ty_zmme_cl.

**------------- atualização zmmt0027
TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_chvw,
         werks TYPE chvw-werks,
         matnr TYPE chvw-matnr,
         charg TYPE chvw-charg,
         budat TYPE chvw-budat,
         menge TYPE chvw-menge,
         bwart TYPE chvw-bwart,
         mblnr TYPE chvw-mblnr,
         mjahr TYPE chvw-mjahr,
       END OF ty_chvw,

       BEGIN OF ty_mseg,
         mblnr TYPE mseg-mblnr,
         mjahr TYPE mseg-mjahr,
         zeile TYPE mseg-zeile,
         bwart TYPE mseg-bwart,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         lgort TYPE mseg-lgort,
         charg TYPE mseg-charg,
         smbln TYPE mseg-smbln,
         menge TYPE mseg-menge,
         budat TYPE mkpf-budat,
         check TYPE c,
       END OF ty_mseg,

       BEGIN OF ty_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         clabs TYPE mchb-clabs,
         cspem TYPE mchb-cspem,
       END OF ty_mchb.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

DATA: estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid VALUE sy-repid,
      tg_sort      TYPE lvc_t_sort,
      wa_sort      LIKE LINE OF tg_sort,
      it_dta       TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE.



*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_data             TYPE TABLE OF type_data,
      t_file             TYPE TABLE OF zmmi0001,
      t_mchb             TYPE TABLE OF type_mchb,
      sl_mchb            TYPE type_mchb,
      w_mchb             TYPE type_mchb,
      t_inob             TYPE TABLE OF type_inob,
      t_kssk             TYPE TABLE OF type_kssk,
      t_klah             TYPE TABLE OF type_klah,
      t_msg              TYPE TABLE OF type_msg,
      t_ksml             TYPE TABLE OF type_ksml,
      t_cabnt            TYPE TABLE OF type_cabnt,
      t_ztfardosmsg      TYPE TABLE OF ztfardosmsg,
      w_ztfardosmsg      TYPE ztfardosmsg,
      st_rmclm           TYPE type_rmclm,
      gw_safrafar_       TYPE ztsafrafardos,
      gw_safrafardos     TYPE ztsafrafardos,
      tw_safrafar_       TYPE TABLE OF ztsafrafardos,
      tw_safrafardos     TYPE TABLE OF ztsafrafardos,
      t_zpp_kuhlmann_hvi TYPE TABLE OF zpp_kuhlmann_hvi,
      result             TYPE zpp_kuhlmann_hvi.


**-------------ATUALIZAÇÃO ZMMT0027
DATA: it_mara       TYPE STANDARD TABLE OF ty_mara,
      it_chvw       TYPE STANDARD TABLE OF ty_chvw,
      it_mseg       TYPE STANDARD TABLE OF ty_mseg,
      it_mseg_aux   TYPE STANDARD TABLE OF ty_mseg,
      it_mchb       TYPE STANDARD TABLE OF ty_mchb,
      it_matnr      TYPE STANDARD TABLE OF zmme_cl,
      it_return     TYPE STANDARD TABLE OF ty_zmme_cl,
      it_return_aux TYPE STANDARD TABLE OF ty_zmme_cl,
      it_zmmt0025   TYPE STANDARD TABLE OF zmmt0025,
      it_zmmt0027   TYPE STANDARD TABLE OF zmmt0027,
      it_zppt0002   TYPE STANDARD TABLE OF zppt0002.  "ADD - IS - 12.06.2013



**------------------ATUALIZAÇÃO ZMMT0027
DATA: wa_mara     TYPE ty_mara,
      wa_chvw     TYPE ty_chvw,
      wa_mseg     TYPE ty_mseg,
      wa_mchb     TYPE ty_mchb,
      wa_matnr    TYPE zmme_cl,
      wa_return   TYPE zmme_cl,
      wa_zmmt0025 TYPE zmmt0025,
      wa_zmmt0027 TYPE zmmt0027,
      wa_zppt0002 TYPE zppt0002.




DATA: vl_fardos      TYPE zmmi0001-fardos.
DATA: vl_ok(1).
"DATA RESULT TYPE ZCL_WEBSERVICE_HVI=>TY_RESULTADO_FARDO.
DATA: vdatai       TYPE sy-datum,
      vdataf       TYPE sy-datum,
      sl_msg       TYPE type_msg,
      vmsg(50),
      vmsgerr(40),
      vlintot      TYPE i,
      vlintotc(10),
      vlinha       TYPE i,
      vlinhac(10),
      valtera      TYPE i,
      valterac(10),
      vachou(1),
      vlogin       TYPE zcl_webservice_hvi=>ty_resultado_fardo-nr_fardo,
      vsenha       TYPE zcl_webservice_hvi=>ty_resultado_fardo-nr_fardo.

*----------ATUALIZAÇÃO ZMMT0027

DATA: s_werks TYPE chvw-werks,
      s_matkl TYPE  mara-matkl,
      s_safra TYPE  zmmt0027-safra.



SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS p_datai TYPE sy-datum OBLIGATORY.
PARAMETERS p_dataf TYPE sy-datum OBLIGATORY.
*-CS2021000759 - 11.10.2021 - JT - inicio
SELECT-OPTIONS p_werks  FOR zppt0002-werks    .
PARAMETERS     p_safra TYPE zppt0002-cd_safra .
*-CS2021000759 - 11.10.2021 - JT - fim
PARAMETERS r_erro  AS CHECKBOX  DEFAULT ' '.
SELECT-OPTIONS: p_dafin  FOR sy-datum MODIF ID a.
SELECTION-SCREEN: END OF BLOCK b1.



AT SELECTION-SCREEN OUTPUT.
  IF p_datai GT p_dataf.
    MESSAGE 'Data inicial maior que final' TYPE 'I'.
    SET CURSOR FIELD 'P_DATAI' .
  ENDIF.

  LOOP AT SCREEN.
    IF r_erro IS INITIAL.
      IF screen-group1 = 'A'.
        REFRESH p_dafin.
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'A'.
        screen-active = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
*

START-OF-SELECTION.

  IF sy-batch IS INITIAL.
    IF p_werks IS INITIAL.
      MESSAGE 'Informar Centro!' TYPE 'I'.
      SET CURSOR FIELD 'P_WERKS' .
      EXIT.

    ENDIF.

    IF p_safra IS INITIAL.
      MESSAGE 'Informar Safra!' TYPE 'I'.
      SET CURSOR FIELD 'P_SAFRA'.
      EXIT.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

  IF r_erro = 'X'.
    PERFORM f_relatorio.
  ELSE.
    PERFORM f_processa_dados.
  ENDIF.


*----------------------------------------------------------------------*
*                               PROCESSAMENTO                          *
*----------------------------------------------------------------------*
FORM f_processa_dados.

  vdatai = p_datai.
  vdataf = p_dataf.

*-CS2021000759 - 11.10.2021 - JT - inicio
  SELECT zpp_kuhlmann_hvi~*
    FROM zpp_kuhlmann_hvi
    INNER JOIN zppt0002 ON zppt0002~cd_sai = zpp_kuhlmann_hvi~fardo
    INTO TABLE @t_zpp_kuhlmann_hvi
*  WHERE data_entrada BETWEEN @p_datai AND @p_dataf
   WHERE data_entrada     >= @p_datai
     AND data_entrada     <= @p_dataf
     AND zppt0002~werks   IN @p_werks
     AND zppt0002~cd_safra = @p_safra.
*-CS2021000759 - 11.10.2021 - JT - fim

*  DATA: OBJ_HVI TYPE REF TO ZCL_WEBSERVICE_HVI.
*  FREE: OBJ_HVI.
*  DATA: TL_MCHB TYPE TABLE OF TYPE_MCHB,
*        TL_KSSK TYPE TABLE OF TYPE_KSSK.
*
*
**********************
****** CRIA OS OBJ
**********************
*  CREATE OBJECT: OBJ_HVI.
*
*  VDATAI = P_DATAI.
*  VDATAF = P_DATAF.
*
  SELECT *
      FROM ztsafrafardos
      INTO TABLE tw_safrafar_
      WHERE login  NE ''
      AND   status = 'L'
      AND werks_to IN p_werks
      AND charg BETWEEN p_datai(4) AND p_dataf(4).        "Provisorio safra Leonardo Portela

  SORT tw_safrafar_ BY matnr werks_to.
  DELETE ADJACENT DUPLICATES FROM tw_safrafar_  COMPARING matnr werks_to login.

  LOOP AT tw_safrafar_ INTO gw_safrafar_.


    REFRESH tw_safrafardos.

    IF sy-batch NE 'X'.
      CONCATENATE 'Buscando dados '  'KULMANN' INTO vmsg SEPARATED BY space.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = vmsg.
    ENDIF.
    "DATA(_RESULT) = OBJ_HVI->BUSCA_MALA_HVI( I_LOGIN = VLOGIN I_SENHA = VSENHA I_DATAI = VDATAI I_DATAF = VDATAF ).
*
    SELECT *
      FROM ztsafrafardos
      INTO TABLE tw_safrafardos
      WHERE matnr    = gw_safrafar_-matnr
      AND   werks_to = gw_safrafar_-werks_to
      AND   login  NE ''
      AND   status = 'L'
      AND charg BETWEEN p_datai(4) AND p_dataf(4).        "Provisorio safra Leonardo Portela

    DESCRIBE TABLE t_zpp_kuhlmann_hvi LINES vlintot.
    WRITE vlintot TO vlintotc.
    CONDENSE vlintotc NO-GAPS.
    CLEAR: valtera, vlinha.
    LOOP AT t_zpp_kuhlmann_hvi INTO result.
      CLEAR vachou.

      REFRESH t_mchb.
      REFRESH t_msg.
      "para cada analise (linha) verifica se lote existe nas safras
      LOOP AT tw_safrafardos INTO gw_safrafardos. "Testa se o fardo HVI é de uma das origem que são beneficiadas
        IF vachou EQ 'X'.
          EXIT.
        ENDIF.

        "Ignora erro se o tamanho do fardo tem 20 posições
        DATA(_tamanho) = strlen( result-fardo ).
        IF _tamanho LT 20.
          vl_fardos = result-fardo.
          CONCATENATE 'Mala:' result-romaneio INTO vmsgerr SEPARATED BY space.
          PERFORM z_retorna_msg_2 USING vmsgerr
                        vl_fardos.
          vachou = 'X'.
          EXIT.
        ENDIF.
        CONCATENATE gw_safrafardos-werks_key
                     result-fardo+12(7) INTO vl_fardos.

        CLEAR: s_safra.
        s_safra = gw_safrafardos-charg.

        SELECT  matnr werks lgort charg clabs
            FROM mchb
               INTO TABLE t_mchb
                  WHERE werks EQ gw_safrafardos-werks_to
                  AND charg EQ vl_fardos.

        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        vachou = 'X'.

        PERFORM: z_seleciona_dados,
                 z_processa_dados.
      ENDLOOP.

      IF vachou NE 'X'.
        PERFORM z_retorna_msg_2 USING text-006
                                vl_fardos.
      ENDIF.

      REFRESH t_ztfardosmsg.
      LOOP AT  t_msg INTO sl_msg.
        MOVE-CORRESPONDING sl_msg TO w_ztfardosmsg.
        w_ztfardosmsg-data_atual = sy-datum.
        w_ztfardosmsg-hora_atual = sy-uzeit.
        TRANSLATE result-data_analise USING '- '.
        CONDENSE result-data_analise NO-GAPS.
        DATA(_tam) = strlen( result-data_analise ).
        IF _tam = 8.
          w_ztfardosmsg-data_arq = result-data_analise.
        ENDIF.
        w_ztfardosmsg-mala     = result-romaneio.
        APPEND w_ztfardosmsg TO t_ztfardosmsg.
      ENDLOOP.
      IF t_ztfardosmsg[] IS NOT INITIAL.
        MODIFY ztfardosmsg FROM TABLE t_ztfardosmsg.
        COMMIT WORK.
      ENDIF.

      IF sy-batch NE 'X'.
        ADD 1 TO  vlinha.
        WRITE vlinha TO vlinhac.
        CONDENSE vlinhac NO-GAPS.
        WRITE valtera TO valterac.
        CONDENSE valterac NO-GAPS.
        CONCATENATE 'Safra/Faz.:' gw_safrafar_-charg '/' gw_safrafar_-werks_to  vlinhac '/' vlintotc ':' valterac INTO vmsg SEPARATED BY space.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            text = vmsg.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleciona Dados                           *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona MCHB
  PERFORM: z_seleciona_mchb,
* Seleciona Dados Características
           z_seleciona_carac.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                             Processa Dados                           *
*----------------------------------------------------------------------*
FORM z_processa_dados.
  DATA: sl_file TYPE zmmi0001.
*        SL_MCHB TYPE TYPE_MCHB.

  SORT: t_inob  BY objek ASCENDING,
        t_kssk  BY objek ASCENDING,
        t_klah  BY clint ASCENDING,
        t_cabnt BY atnam ASCENDING.

  sl_file-fardos    = vl_fardos. CONDENSE sl_file-fardos. " RESULT-NR_FARDO+12(7).
  sl_file-uhml      = result-hvi_pol. CONDENSE sl_file-uhml.
  sl_file-ui        = result-hvi_unf. CONDENSE sl_file-ui.
  sl_file-str       = result-hvi_str. CONDENSE sl_file-str.
  sl_file-elg       = result-hvi_elg. CONDENSE sl_file-elg.
  sl_file-mic       = result-hvi_mic. CONDENSE sl_file-mic.
  sl_file-rd        = result-hvi_rd.  CONDENSE sl_file-rd.
  sl_file-plusb     = result-hvi_b.   CONDENSE sl_file-plusb.
  sl_file-cg        = result-hvi_cg.  CONDENSE sl_file-cg.
  sl_file-t_cnt     = result-hvi_count. CONDENSE sl_file-t_cnt.
  sl_file-t_area    = result-hvi_are. CONDENSE sl_file-t_area.
  sl_file-leaf      = result-hvi_leaf.CONDENSE sl_file-leaf.
  sl_file-mr        = result-hvi_mat. CONDENSE sl_file-mr.
  sl_file-sfi_w     = result-hvi_sfi. CONDENSE sl_file-sfi_w.
  sl_file-sci       = result-hvi_sci. CONDENSE sl_file-sci.
  sl_file-csp       = result-hvi_csp. CONDENSE sl_file-csp.
*     Executa Bapi MSC2N
  LOOP AT t_mchb INTO sl_mchb WHERE charg EQ sl_file-fardos.
    IF sl_mchb-clabs GT 0.
      PERFORM z_executa_msc2n USING sl_file
                                    sl_mchb.


*      S_WERKS = SL_MCHB-WERKS.
      "Seleciona os dados.


*      PERFORM F_SEL_DADOS_ATUA_ZMMT0027.

      "Trata as informações.
*      PERFORM F_MONTA_TABELA_ZMMT0027.

*      "Insere os dados na tabela.
*      PERFORM F_INSERIR_TABELA_ZMMT0027.
    ENDIF.
  ENDLOOP.


  "Atualizacao tabela zmmt0027




ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTA_MSC2N                                          *
*&---------------------------------------------------------------------*
*                         Executa Bapi MSC2N                           *
*----------------------------------------------------------------------*
FORM z_executa_msc2n USING p_file TYPE zmmi0001
                           p_mchb TYPE type_mchb.

  DATA: vl_num   TYPE bapi1003_key-classnum,
        vl_type  TYPE bapi1003_key-classtype,
        vl_table TYPE bapi1003_key-objecttable,
        vl_key   TYPE bapi1003_key-object,
        sl_keys  TYPE bapi1003_object_keys,
        sl_inob  TYPE type_inob,
        sl_kssk  TYPE type_kssk,
        sl_klah  TYPE type_klah,
        tl_table TYPE TABLE OF bapi1003_object_keys,
        tl_ret   TYPE TABLE OF bapiret2,
        tl_num   TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char  TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr  TYPE TABLE OF bapi1003_alloc_values_curr.

  REFRESH: tl_table, tl_ret.
  CLEAR: sl_inob, sl_kssk, sl_klah.

  sl_keys-key_field = 'MATNR'.
  sl_keys-value_int = p_mchb-matnr.
  APPEND sl_keys TO tl_table.

  sl_keys-key_field = 'CHARG'.
  sl_keys-value_int = p_mchb-charg.
  APPEND sl_keys TO tl_table.

*  sl_keys-key_field = 'WERKS'.
*  sl_keys-value_int = p_mchb-werks.
*  APPEND sl_keys TO tl_table.

  vl_table = 'MCH1'.

  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
    EXPORTING
      objecttable    = vl_table
    IMPORTING
      objectkey_conc = vl_key
    TABLES
      objectkeytable = tl_table
      return         = tl_ret.

  CHECK sy-subrc IS INITIAL.

  READ TABLE t_inob INTO sl_inob WITH KEY objek = p_mchb-objek BINARY SEARCH.

  READ TABLE t_kssk INTO sl_kssk WITH KEY objek = sl_inob-objekk BINARY SEARCH.

  READ TABLE t_klah INTO sl_klah WITH KEY clint = sl_kssk-clint BINARY SEARCH.

  vl_num  = sl_klah-class.
  vl_type = st_rmclm-klart.

* Preenche Características
  CLEAR vl_ok.
  PERFORM z_preenche_carac TABLES tl_char
                                  tl_num
                            USING p_file
                                  vl_key
                                  vl_table
                                  vl_num
                                  vl_type.

  CHECK vl_ok IS INITIAL.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE' "#EC CI_USAGE_OK[2438131] ---> S4 MIGRATION 06/07/2023 - MA
    EXPORTING
      objectkey          = vl_key
      objecttable        = vl_table
      classnum           = vl_num
      classtype          = vl_type
    TABLES
      allocvaluesnumnew  = tl_num
      allocvaluescharnew = tl_char
      allocvaluescurrnew = tl_curr
      return             = tl_ret.

*  ADD 1 TO VALTERA.
  DELETE tl_ret WHERE type EQ 'S'.
  DELETE tl_ret WHERE type EQ 'W'.
* Retorna MSG
  PERFORM z_retorna_msg TABLES tl_ret
                         USING p_mchb-matnr
                               p_mchb-charg
                               p_mchb-werks.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " Z_EXECUTA_MSC2N

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_CARAC                                         *
*&---------------------------------------------------------------------*
*                      Preenche Características                        *
*----------------------------------------------------------------------*
FORM z_preenche_carac TABLES p_char  STRUCTURE bapi1003_alloc_values_char
                             p_tnum  STRUCTURE bapi1003_alloc_values_num
                       USING p_file  TYPE zmmi0001
                             p_key   TYPE bapi1003_key-object
                             p_table TYPE bapi1003_key-objecttable
                             p_num   TYPE bapi1003_key-classnum
                             p_type  TYPE bapi1003_key-classtype.

  DATA: tl_cat    TYPE lvc_t_fcat,
        sl_cat    TYPE lvc_s_fcat,
        vl_campo  TYPE char30,
        sl_char   TYPE bapi1003_alloc_values_char,
        sl_num    TYPE bapi1003_alloc_values_num,
        sl_aux    TYPE bapi1003_alloc_values_char,
        sl_cabnt  TYPE type_cabnt,
        tl_aux    TYPE TABLE OF bapi1003_alloc_values_char,
        tl_num    TYPE TABLE OF bapi1003_alloc_values_num,
        tl_char   TYPE TABLE OF bapi1003_alloc_values_char,
        tl_curr   TYPE TABLE OF bapi1003_alloc_values_curr,
        tl_ret    TYPE TABLE OF bapiret2,
        n_uhml(8) TYPE p DECIMALS 2,
        f_novo(1),
        vlok      TYPE i.

  " FIELD-SYMBOLS <CAMPO> TYPE CHAR10.
  FIELD-SYMBOLS <campo> TYPE any. "Leonardo Portela

  REFRESH: p_char, p_tnum.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL' "#EC CI_USAGE_OK[2438131] ---> S4 MIGRATION 06/07/2023 - MA
    EXPORTING
      objectkey       = p_key
      objecttable     = p_table
      classnum        = p_num
      classtype       = p_type
    TABLES
      allocvaluesnum  = tl_num
      allocvalueschar = tl_char
      allocvaluescurr = tl_curr
      return          = tl_ret.

  DESCRIBE TABLE tl_char LINES vlok.
*  IF VLOK GE 14.
*    VL_OK = 'X'.
*    EXIT.
*  ENDIF.

  SORT: tl_num  BY charact ASCENDING,
        tl_char BY charact ASCENDING.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMMI0001'
    CHANGING
      ct_fieldcat            = tl_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DELETE tl_cat INDEX 1.

  CLEAR f_novo.
  LOOP AT tl_cat INTO sl_cat.
    CLEAR: vl_campo, sl_aux.

    CONCATENATE 'P_FILE' sl_cat-fieldname INTO vl_campo SEPARATED BY '-'.

    ASSIGN (vl_campo) TO <campo>.

    CHECK <campo> IS ASSIGNED.

    CASE sl_cat-fieldname.
      WHEN 'PLUSB'.
        sl_cat-fieldname = '+B'.
      WHEN 'T_CNT'.
        sl_cat-fieldname = 'T.CNT'.
      WHEN 'T_AREA'.
        sl_cat-fieldname = 'T.AREA'.
      WHEN 'SFI_W'.
        sl_cat-fieldname = 'SFI(W)'.
      WHEN 'UHML'.

    ENDCASE.

    sl_aux-charact_descr  = sl_cat-fieldname.
    sl_aux-value_char     = <campo>.

    APPEND sl_aux TO tl_aux.

    CLEAR sl_cat.
    UNASSIGN <campo>.
  ENDLOOP.

  SORT tl_aux BY charact_descr ASCENDING.

  LOOP AT t_cabnt INTO sl_cabnt.
    CLEAR: p_char, p_tnum, sl_aux.

    READ TABLE tl_aux INTO sl_aux WITH KEY charact_descr = sl_cabnt-atbez BINARY SEARCH.
    IF NOT sl_aux-value_char IS INITIAL.
      "ALRS Preenche se estiver em branco e mantem o antigo
      READ TABLE tl_char INTO sl_char WITH KEY charact = sl_cabnt-atnam BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF sl_char-value_char IS NOT INITIAL.
*          Alterado devido ao fardinhos ja possuerem classificação HVI.  "AOENNING
          sl_aux-value_char = sl_char-value_char.
        ELSE.
          f_novo = 'X'.
        ENDIF.
      ELSE.
        f_novo = 'X'.
      ENDIF.
      "ALRS
      IF sl_cabnt-atbez = 'UHML'. "polegadas
        n_uhml = sl_aux-value_char.
        IF n_uhml GT 10. "se maior que 10mm converte
          n_uhml = ( n_uhml * 10 ) / 254.
          WRITE n_uhml TO sl_aux-value_char.
          TRANSLATE sl_aux-value_char USING ',.'.
          CONDENSE sl_aux-value_char NO-GAPS.
        ENDIF.
      ENDIF.

      CASE sl_cabnt-atfor.
        WHEN 'CHAR'.
          p_char-charact     = sl_cabnt-atnam.
          p_char-value_char  = sl_aux-value_char.
          APPEND p_char.
        WHEN 'NUM'.
          p_tnum-charact     = sl_cabnt-atnam.
          p_tnum-value_from  = sl_aux-value_char.
          APPEND p_tnum.
      ENDCASE.
    ELSE.
      CASE sl_cabnt-atbez.
*        WHEN 'Safra'.
*          READ TABLE tl_num INTO sl_num WITH KEY charact = sl_cabnt-atnam BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*            p_tnum-charact     = sl_cabnt-atnam.
*            p_tnum-value_from  = sl_num-value_from.
*            APPEND p_tnum.
*          ENDIF.
        WHEN 'Variedade' OR 'Talhao'
          OR 'Safra' OR 'Periodo' OR 'Mala'.    "ADD - 21.06.2013

          READ TABLE tl_char INTO sl_char WITH KEY charact = sl_cabnt-atnam BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            p_char-charact     = sl_cabnt-atnam.
            p_char-value_char  = sl_char-value_char.
            APPEND p_char.
          ENDIF.
      ENDCASE.
    ENDIF.

    CLEAR: sl_cabnt, sl_aux, sl_num, sl_char.
  ENDLOOP.

  IF f_novo = 'X'.
    ADD 1 TO valtera.
  ELSE.
*    CLEAR: VL_OK.
    vl_ok = 'X'.
  ENDIF.

ENDFORM.                    " Z_PREENCHE_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_MCHB                                         *
*&---------------------------------------------------------------------*
*                            Seleciona MCHB                            *
*----------------------------------------------------------------------*
FORM z_seleciona_mchb.

  SORT t_mchb BY charg ASCENDING
                 matnr ASCENDING.

  DELETE ADJACENT DUPLICATES FROM t_mchb COMPARING charg matnr.

* Move Matnr p/ Objek
  PERFORM z_move_matnr_objek.

ENDFORM.                    " Z_SELECIONA_MCHB

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_MATNR_OBJEK                                       *
*&---------------------------------------------------------------------*
*                          Move Matnr p/ Objek                         *
*----------------------------------------------------------------------*
FORM z_move_matnr_objek.

  DATA: sl_mchb  TYPE type_mchb,
        vl_index TYPE i.

  LOOP AT t_mchb INTO sl_mchb.
    vl_index = sy-tabix.

    sl_mchb-objek = sl_mchb-matnr.

    MODIFY t_mchb FROM sl_mchb INDEX vl_index TRANSPORTING objek.

    CLEAR sl_mchb.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_MATNR_OBJEK

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_CARAC                                        *
*&---------------------------------------------------------------------*
*                     Seleciona Dados Características                  *
*----------------------------------------------------------------------*
FORM z_seleciona_carac.
  DATA: tl_mchb TYPE TABLE OF type_mchb,
        tl_kssk TYPE TABLE OF type_kssk.

  REFRESH: t_inob, t_kssk, t_klah, t_ksml, t_cabnt.
  CLEAR st_rmclm.

  CHECK NOT t_mchb[] IS INITIAL.
  tl_mchb[] = t_mchb[].
  SORT tl_mchb BY objek ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_mchb COMPARING objek.

  SELECT SINGLE a~klart FROM tcla AS a
    INNER JOIN tclao AS b ON a~klart EQ b~klart
    INTO st_rmclm-klart
  WHERE a~obtab    EQ 'MCHA'
    AND a~intklart EQ space
    AND a~multobj  EQ 'X'
  AND b~obtab    EQ 'MCH1'.

  SELECT cuobj klart obtab objek
  FROM inob
    INTO TABLE t_inob
    FOR ALL ENTRIES IN tl_mchb
   WHERE klart EQ st_rmclm-klart
     AND obtab EQ 'MARA'
  AND objek EQ tl_mchb-objek.

  CHECK NOT t_inob[] IS INITIAL.
* Move Cuobj p/ Objekk
  PERFORM z_move_cuobj_objekk.

  SELECT objek mafid klart clint adzhl
  FROM kssk
    INTO TABLE t_kssk
    FOR ALL ENTRIES IN t_inob
  WHERE objek EQ t_inob-objekk
    AND mafid EQ 'O'
  AND klart EQ st_rmclm-klart.

  CHECK NOT t_kssk[] IS INITIAL.
  tl_kssk[] = t_kssk[].
  SORT tl_kssk BY clint ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_kssk COMPARING clint.

  SELECT clint klart class vondt bisdt
  FROM klah
    INTO TABLE t_klah
    FOR ALL ENTRIES IN tl_kssk
  WHERE clint EQ tl_kssk-clint.

  SELECT clint posnr adzhl imerk klart
  FROM ksml
    INTO TABLE t_ksml
    FOR ALL ENTRIES IN tl_kssk
  WHERE clint EQ tl_kssk-clint
  AND klart EQ st_rmclm-klart.

  CHECK NOT t_ksml[] IS INITIAL.

  SELECT a~atinn a~spras a~adzhl a~atbez
         b~atnam b~atfor
  FROM cabnt AS a
    INNER JOIN cabn AS b ON a~atinn EQ b~atinn
                        AND a~adzhl EQ b~adzhl
    INTO TABLE t_cabnt
    FOR ALL ENTRIES IN t_ksml
  WHERE a~atinn EQ t_ksml-imerk
  AND a~spras EQ 'PT'.

ENDFORM.                    " Z_SELECIONA_CARAC

*&---------------------------------------------------------------------*
*&      Form  Z_MOVE_CUOBJ_OBJEKK                                      *
*&---------------------------------------------------------------------*
*                          Move Cuobj p/ Objekk                        *
*----------------------------------------------------------------------*
FORM z_move_cuobj_objekk.
  DATA: sl_inob  TYPE type_inob,
        vl_index TYPE i.

  LOOP AT t_inob INTO sl_inob.
    vl_index = sy-tabix.

    sl_inob-objekk = sl_inob-cuobj.

    MODIFY t_inob FROM sl_inob INDEX vl_index TRANSPORTING objekk.

    CLEAR sl_inob.
  ENDLOOP.

ENDFORM.                    " Z_MOVE_CUOBJ_OBJEKK


FORM f_sel_dados_atua_zmmt0027.

  FIELD-SYMBOLS: <fs_return> TYPE ty_zmme_cl,
                 <fs_mseg>   TYPE ty_mseg.

  SELECT SINGLE
  matkl FROM mara
  INTO s_matkl
  WHERE matnr EQ sl_mchb-matnr.

** LIMPAR TABELA ZMMT0027 OS PARAMETROS REQUISITADOS JA EXISTIREM
  DELETE FROM zmmt0027 WHERE werks  EQ s_werks
                         AND matkl  EQ s_matkl
                         AND ( safra  EQ s_safra OR safra EQ '' ).

  PERFORM f_progress USING 10 'Selecionando lotes.'.

** SELECIONAR MATERIAIS
  SELECT matnr matkl normt
    FROM mara
    INTO TABLE it_mara
   WHERE matkl EQ s_matkl.

  IF it_mara[] IS NOT INITIAL.
** SELECIONAR OS TIPOS DE ATINN
    SELECT *
      FROM zmmt0025
      INTO TABLE it_zmmt0025.

    IF it_zmmt0025[] IS NOT INITIAL.
** SELECIONAR LOTES DE ACORDO COM OS MATERIAIS
      SELECT SINGLE data_inicio data_fim
      FROM ztsafrafardos
        INTO (p_datai, p_dataf)
      WHERE charg EQ s_safra.

      IF p_datai IS NOT INITIAL AND p_dataf IS NOT INITIAL.
        SELECT m~mblnr m~mjahr m~zeile m~bwart m~matnr
               m~werks m~lgort m~charg m~smbln m~menge
               k~budat
          FROM mseg AS m
         INNER JOIN mkpf AS k  ON  k~mblnr = m~mblnr
                               AND k~mjahr = m~mjahr
          INTO TABLE it_mseg
           FOR ALL ENTRIES IN it_mara
         WHERE m~werks EQ s_werks
           AND m~matnr EQ it_mara-matnr
           AND k~budat BETWEEN p_datai AND p_dataf
           AND m~bwart IN ('131','309','311')
           AND m~shkzg EQ 'S'.

        IF it_mseg[] IS NOT INITIAL.
** DELETAR DOCUMENTOS ESTORNADOS
          PERFORM f_progress USING 25 'Verificando estornados.'.

          SELECT mblnr mjahr zeile bwart matnr
                 werks lgort charg smbln menge
                 budat_mkpf
            FROM mseg
            INTO TABLE it_mseg_aux
             FOR ALL ENTRIES IN it_mseg
           WHERE mjahr EQ it_mseg-mjahr
             AND smbln EQ it_mseg-mblnr.

          IF it_mseg_aux[] IS NOT INITIAL.
            SORT it_mseg_aux BY mjahr smbln.

            LOOP AT it_mseg_aux INTO wa_mseg.
              DELETE it_mseg WHERE mjahr EQ wa_mseg-mjahr
                               AND mblnr EQ wa_mseg-smbln.

            ENDLOOP.

          ENDIF.

          IF it_mseg[] IS NOT INITIAL.
** LOTES DUPLICADOS
            SORT it_mseg BY werks charg mblnr DESCENDING.
            DELETE ADJACENT DUPLICATES FROM it_mseg COMPARING  werks charg.

** SELECIONAR SAFRA, DADOS CLASSIFICAÇÃO
            PERFORM f_progress USING 40 'Buscando classificação.'.

            LOOP AT it_mseg INTO wa_mseg.
              MOVE: wa_mseg-matnr TO wa_matnr-matnr,
                    wa_mseg-charg TO wa_matnr-charg.

              APPEND wa_matnr TO it_matnr.

            ENDLOOP.

            CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
              TABLES
                t_matnr  = it_matnr
                t_return = it_return.

            IF it_return[] IS NOT INITIAL.
              SORT: it_zmmt0025 BY atnam,
                    it_return BY matnr charg atinn.

              READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.

              LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                                 AND atwrt NE s_safra.
                APPEND wa_return TO it_return_aux.

              ENDLOOP.

              IF it_return_aux[] IS NOT INITIAL.
                SORT it_return_aux BY matnr charg atinn.

                CLEAR: wa_return.

                LOOP AT it_return_aux INTO wa_return.
                  READ TABLE it_return ASSIGNING <fs_return> WITH KEY matnr = wa_return-matnr
                                                                      charg = wa_return-charg.
                  IF sy-subrc IS INITIAL.
                    <fs_return>-check = abap_true.
                  ENDIF.

                  READ TABLE it_mseg ASSIGNING <fs_mseg> WITH KEY matnr = wa_return-matnr
                                                                  charg = wa_return-charg.
                  IF sy-subrc IS INITIAL.
                    <fs_mseg>-check = abap_true.
                  ENDIF.

                ENDLOOP.

                UNASSIGN: <fs_mseg>, <fs_return>.

                DELETE it_return WHERE check EQ abap_true.
                DELETE it_mseg WHERE check EQ abap_true.

              ENDIF.

            ENDIF.

** SELEÇÕES FARDO DE ORIGEM
** ADD - IS - 12.06.2013 - Inicio
            SELECT *
              FROM zppt0002
              INTO TABLE it_zppt0002
               FOR ALL ENTRIES IN it_mseg
             WHERE acharg  EQ it_mseg-charg
               AND werks   EQ it_mseg-werks.

            SORT it_zppt0002 BY acharg werks .
** ADD - IS - 12.06.2013 - Fim

** SELEÇAO DA COLUNA “STATUS”
            PERFORM f_progress USING 55 'Verificando status.'.

            SELECT matnr werks lgort charg clabs cspem
              FROM mchb
              INTO TABLE it_mchb
               FOR ALL ENTRIES IN it_mseg
             WHERE matnr EQ it_mseg-matnr
               AND werks EQ it_mseg-werks
               AND lgort EQ it_mseg-lgort
               AND charg EQ it_mseg-charg.

** TABELA MSEG VAZIA APOS ESTORNADOS
*            MESSAGE 'Documentos estornados.' TYPE 'I'.
          ENDIF.
        ELSE.
** NAO FORAM ENCONTRADOS DADOS NA TABELA MSEG
          MESSAGE 'Não foram encontrados dados para os parâmetros informados.' TYPE 'I'.

        ENDIF.

      ELSE.
** NAO EXISTE DADOS NA TABELA ZTSAFRAFARDOS
        MESSAGE 'Não foram encontrados dados na tabela ZTSAFRAFARDOS para a SAFRA informada.' TYPE 'I'.

      ENDIF.

    ELSE.
** NAO EXISTE DADOS NA TABELA ZZMMT0025
      MESSAGE 'Não foram encontrados dados na tabela ZMMT0025.' TYPE 'I'.

    ENDIF.

  ELSE.
** NÃO EXISTE MATERIAIS PARA O GRUPO(S) INFORMADO(S)
    MESSAGE 'Não foram encontrados materiais para o grupo(s) informado(s).' TYPE 'I'.

  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS_ATUALIZACAO_ZMMT0027



FORM f_monta_tabela_zmmt0027.
  CHECK: it_mseg[] IS NOT INITIAL.

  DATA: vl_tabix    TYPE sy-tabix.

  PERFORM f_progress USING 70 'Montando tabela.'.

  SORT: it_mara     BY matnr normt matkl,
*        it_mseg     BY mblnr mjahr matnr werks lgort charg smbln,
        it_mseg     BY matnr werks lgort charg,
        it_mchb     BY matnr werks lgort charg clabs cspem,
        it_return   BY matnr charg atinn,
        it_zmmt0025 BY atinn.


  CLEAR: wa_mseg.
  LOOP AT it_mseg INTO wa_mseg.
    CLEAR: wa_mara, wa_mchb, wa_return, wa_zmmt0027, wa_zppt0002.

    wa_zmmt0027-werks   = wa_mseg-werks.
    wa_zmmt0027-matnr   = wa_mseg-matnr.
    wa_zmmt0027-charg   = wa_mseg-charg.
    wa_zmmt0027-menge   = wa_mseg-menge.
    wa_zmmt0027-budat   = wa_mseg-budat.
    wa_zmmt0027-lgort   = wa_mseg-lgort.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_mseg-matnr
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zmmt0027-matkl   = wa_mara-matkl.
      wa_zmmt0027-normt   = wa_mara-normt.
    ENDIF.

** ADD - IS - 12.06.2013 - Inicio
    READ TABLE it_zppt0002 INTO wa_zppt0002 WITH KEY acharg = wa_mseg-charg
                                                     werks  = wa_mseg-werks
*                                                     matnr  = wa_mseg-matnr
                                            BINARY SEARCH.
    IF sy-subrc = 0.
      wa_zmmt0027-charg_orig  = wa_zppt0002-charg.
    ENDIF.
** ADD - IS - 12.06.2013 - Fim

    READ TABLE it_mchb INTO wa_mchb WITH KEY matnr = wa_mseg-matnr
                                             werks = wa_mseg-werks
                                             lgort = wa_mseg-lgort
                                             charg = wa_mseg-charg
                                    BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_mchb-clabs EQ 0 AND wa_mchb-cspem EQ 0.
        wa_zmmt0027-status = 'EMBARCADO'.
      ELSEIF wa_mchb-cspem NE 0.
        wa_zmmt0027-status = 'RESERVADO'.
      ELSEIF wa_mchb-clabs NE 0.
        wa_zmmt0027-status = 'DISPONIVEL'.
      ELSEIF wa_mchb-cspem NE 0 AND wa_mchb-clabs NE 0.
        wa_zmmt0027-status = 'VERIFICAR'.
      ENDIF.
    ENDIF.

**    LOOP AT it_return INTO wa_return WHERE matnr = wa_mseg-matnr
**                                       AND charg = wa_mseg-charg.
    READ TABLE it_return INTO wa_return WITH KEY matnr = wa_mseg-matnr
                                                 charg = wa_mseg-charg
                                        BINARY SEARCH.
    IF sy-subrc = 0.
      vl_tabix = sy-tabix.

      LOOP AT it_return INTO wa_return FROM vl_tabix.
        IF    wa_return-matnr <> wa_mseg-matnr
          OR  wa_return-charg <> wa_mseg-charg.
          EXIT.
        ENDIF.

        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atinn = wa_return-atinn
                                                BINARY SEARCH.
        IF sy-subrc = 0.
          CASE wa_zmmt0025-atnam.
            WHEN: 'UHML'.
              wa_zmmt0027-far_uhml    = wa_return-atwrt.      " FARDINHOS_UHML
            WHEN: 'UI'.
              wa_zmmt0027-far_ui      = wa_return-atwrt.      " FARDINHOS_UI
            WHEN: 'STR'.
              wa_zmmt0027-far_str     = wa_return-atwrt.      " FARDINHOS_STR
            WHEN: 'ELG'.
              wa_zmmt0027-far_elg     = wa_return-atwrt.      " FARDINHOS_ELG
            WHEN: 'MIC'.
              wa_zmmt0027-far_mic     = wa_return-atwrt.      " FARDINHOS_MIC
            WHEN: 'RD'.
              wa_zmmt0027-far_rd      = wa_return-atwrt.      " FARDINHOS_RD
            WHEN: '+B'.
              wa_zmmt0027-far_b       = wa_return-atwrt.      " FARDINHOS_B
            WHEN: 'CG'.
              wa_zmmt0027-far_cg      = wa_return-atwrt.      " FARDINHOS_CG
            WHEN: 'T.CNT'.
              wa_zmmt0027-far_tcnt    = wa_return-atwrt.      " FARDINHOS_TCNT
            WHEN: 'T.AREA'.
              wa_zmmt0027-far_tarea   = wa_return-atwrt.      " FARDINHOS_TAREA
            WHEN: 'LEAF'.
              wa_zmmt0027-far_leaf    = wa_return-atwrt.      " FARDINHOS_LEAF
            WHEN: 'MR'.
              wa_zmmt0027-far_mr      = wa_return-atwrt.      " FARDINHOS_MR
            WHEN: 'SFI(W)'.
              wa_zmmt0027-far_sfiw    = wa_return-atwrt.      " FARDINHOS_SFIW
            WHEN: 'SCI'.
              wa_zmmt0027-far_sci     = wa_return-atwrt.      " FARDINHOS_SCI
            WHEN: 'CSP'.
              wa_zmmt0027-far_csp     = wa_return-atwrt.      " FARDINHOS_CSP
            WHEN: 'PERIODO'.
              wa_zmmt0027-far_periodo = wa_return-atwrt.      " FARDINHOS_PERIODO
            WHEN: 'SAFRA'.
              wa_zmmt0027-safra       = wa_return-atwrt.      " SAFRA
            WHEN: 'VARIEDADE'.
              wa_zmmt0027-variedade   = wa_return-atwrt.      " VARIEDADE
            WHEN: 'TALHAO'.
              wa_zmmt0027-talhao      = wa_return-atwrt.      " TALHAO
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
**    ENDLOOP.
    APPEND wa_zmmt0027 TO it_zmmt0027.
  ENDLOOP.

ENDFORM.                    " F_MONTA_TABELA_ZMMT0027


FORM f_inserir_tabela_zmmt0027 .
  CHECK: it_zmmt0027[] IS NOT INITIAL.

  PERFORM f_progress USING 95 'Gravando dados.'.

  SORT it_zmmt0027 BY charg werks matkl matnr safra.
  MODIFY zmmt0027 FROM TABLE it_zmmt0027.

  IF sy-subrc IS INITIAL.
    COMMIT WORK.
    MESSAGE 'Dados inseridos com sucesso na tabela ZMMT0027' TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Não foi possivel inserir os dados na tabela ZMMT0027' TYPE 'I'.
  ENDIF.

ENDFORM.                    " F_INSERIR_TABELA_ZMMT0027

*----------------------------------------------------------------------*
FORM f_progress  USING vf_percen vf_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = vf_percen   " Veloc do relogio em %
      text       = vf_text.    " Texto que aparecerá.
ENDFORM.                    " F_PROGRESS_ZMMT0027

*&---------------------------------------------------------------------*
*&      Form  Z_TRCA_P_V                                               *
*&---------------------------------------------------------------------*
*                               Troca , por .                          *
*----------------------------------------------------------------------*
FORM z_trca_p_v CHANGING p_campo TYPE char10.
  REPLACE ',' IN p_campo WITH '.'.
ENDFORM.                    " Z_TRCA_P_V

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG                                            *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM z_retorna_msg TABLES p_ret   STRUCTURE bapiret2
                    USING p_matnr TYPE mchb-matnr
                          p_charg TYPE mchb-charg
                          p_werks TYPE mchb-werks.

  DATA: sl_ret TYPE bapiret2.


  LOOP AT p_ret INTO sl_ret.
    sl_msg-material = p_matnr.
    sl_msg-lote     = p_charg.
    sl_msg-centro   = p_werks.
    sl_msg-tipo     = sl_ret-type.
    sl_msg-numero   = sl_ret-number.
    sl_msg-mensagem = sl_ret-message.

    APPEND sl_msg TO t_msg.

    CLEAR: sl_ret, sl_msg.
  ENDLOOP.

ENDFORM.                    " Z_RETORNA_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_RETORNA_MSG_2                                          *
*&---------------------------------------------------------------------*
*                               Retorna MSG                            *
*----------------------------------------------------------------------*
FORM z_retorna_msg_2 USING p_text  TYPE c
                           p_charg TYPE mchb-charg.

  DATA sl_msg TYPE type_msg.

  sl_msg-lote     = p_charg.
  sl_msg-centro   = gw_safrafardos-werks_to.
  sl_msg-tipo     = 'E'.
  sl_msg-numero   =  1.
  sl_msg-mensagem  = p_text.

  APPEND sl_msg TO t_msg.

ENDFORM.                    " Z_RETORNA_MSG_2
*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_relatorio .
  SELECT *
  FROM ztfardosmsg
  INTO TABLE t_ztfardosmsg
  WHERE data_atual BETWEEN p_datai AND p_dataf
  AND   data_arq  IN p_dafin.

  PERFORM montar_layout.
  DATA: wl_layout TYPE  slis_layout_alv.
  wl_layout-zebra = 'X'.
  wl_layout-window_titlebar = 'Relatorio de Erros KUHLMANN'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      is_layout          = wl_layout
      it_fieldcat        = estrutura[]
*     IT_SORT            = T_SORT[]
      i_default          = ' '
      i_save             = ' '
*     I_SCREEN_START_COLUMN = 3
*     I_SCREEN_START_LINE   = 3
*     I_SCREEN_END_COLUMN   = 60
*     I_SCREEN_END_LINE  = 13
    TABLES
      t_outtab           = t_ztfardosmsg.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH: estrutura.

  PERFORM montar_estrutura USING:

        1 ' '         ' '           'T_ZTFARDOSMSG' 'DATA_ATUAL' 'Data Proc'       '15'  ' ' ' ' ' ' ,
        2 ' '         ' '           'T_ZTFARDOSMSG' 'DATA_ARQ'   'Data Final'      '15'  ' ' ' ' ' ' ,
        3 ' '         ' '           'T_ZTFARDOSMSG' 'LOTE'       'Lote'            '15'  ' ' ' ' ' ' ,
        4 ' '         ' '           'T_ZTFARDOSMSG' 'MATERIAL'   'Material'        '15'  ' ' ' ' ' ' ,
        4 ' '         ' '           'T_ZTFARDOSMSG' 'MENSAGEM'   'Mensagem'        '50'  ' ' ' ' ' ' .
ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-edit          = p_edit.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  wa_estrutura-outputlen     = p_outputlen. "X_CONTADOR.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
