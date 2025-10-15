*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski                                      &*
*& Data.....: 25/10/2013                                              &*
*& Descrição: Indicadores de Suprimentos                              &*
*& Transação: ZMMR039                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zmmr042.
TABLES: ekko.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.

TYPES: BEGIN OF ty_eban,
         matnr    TYPE eban-matnr,
         werks    TYPE eban-werks,
         badat    TYPE eban-badat,
         banfn    TYPE eban-banfn,
         bnfpo    TYPE eban-bnfpo,
         ernam    TYPE eban-ernam,
         frgdt    TYPE eban-frgdt,
         frgst    TYPE eban-frgst,
         txz01    TYPE eban-txz01,
         packno   TYPE eban-packno,
         pstyp    TYPE eban-pstyp,
         knttp    TYPE eban-knttp,
         lfdat    TYPE eban-lfdat,
         lgort    TYPE eban-lgort,
         objectid TYPE cdhdr-objectid,
         konnr    TYPE eban-konnr,
         prio_urg TYPE eban-prio_urg,
       END OF ty_eban,

       BEGIN OF ty_t16fc,
         frgco TYPE t16fc-frgco,
*        FRGCT TYPE T16FC-FRGCT,
       END OF ty_t16fc,

       BEGIN OF ty_t16fd,
         frgco TYPE t16fd-frgco,
         frgct TYPE t16fd-frgct,
       END OF ty_t16fd,

       BEGIN OF ty_ekpo,
         banfn    TYPE ekpo-banfn,
         bnfpo    TYPE ekpo-bnfpo,
         ebeln    TYPE ekpo-ebeln,
         ebelp    TYPE ekpo-ebelp,
         matnr    TYPE ekpo-matnr,
         txz01    TYPE ekpo-txz01,
         werks    TYPE ekpo-werks,
         objectid TYPE cdhdr-objectid,
         pstyp    TYPE ekpo-pstyp,
         knttp    TYPE ekpo-knttp,
         packno   TYPE ekpo-packno,
         konnr    TYPE ekpo-konnr,
       END OF ty_ekpo,

       BEGIN OF ty_ekko,
         ebeln TYPE ekko-ebeln,
         bsart TYPE ekko-bsart,
         aedat TYPE ekko-aedat,
         ernam TYPE ekko-ernam,
         lifnr TYPE ekko-lifnr,
         ekgrp TYPE ekko-ekgrp,
       END OF ty_ekko,

       BEGIN OF ty_t024,
         ekgrp TYPE t024-ekgrp,
         eknam TYPE t024-eknam,
       END OF ty_t024,

       BEGIN OF ty_cdhdr,
         objectclas TYPE cdhdr-objectclas,
         objectid   TYPE cdhdr-objectid,
         changenr   TYPE cdhdr-changenr,
         tcode      TYPE cdhdr-tcode,
         username   TYPE cdhdr-username,
         udate      TYPE cdhdr-udate,
         utime      TYPE cdhdr-utime,
       END OF ty_cdhdr,

       BEGIN OF ty_cdpos,
         objectclas TYPE cdpos-objectclas,
         objectid   TYPE cdpos-objectid,
         changenr   TYPE cdpos-changenr,
         fname      TYPE cdpos-fname,
         value_new  TYPE cdpos-value_new,
         tabkey     TYPE cdpos-tabkey,
       END OF ty_cdpos,

       BEGIN OF ty_eket,
         ebeln TYPE eket-ebeln,
         ebelp TYPE eket-ebelp,
         eindt TYPE eket-eindt,
       END OF ty_eket,

       BEGIN OF ty_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         vgabe TYPE ekbe-vgabe,
         shkzg TYPE ekbe-shkzg,
         belnr TYPE ekbe-belnr,
         budat TYPE ekbe-budat,
         ernam TYPE ekbe-ernam,
       END OF ty_ekbe,

       BEGIN OF ty_mseg,
         smbln TYPE mseg-smbln,
       END OF ty_mseg,

       BEGIN OF ty_zmmt0042,
         ebeln      TYPE zmmt0042-ebeln,
         ebelp      TYPE zmmt0042-ebelp,
         data_atual TYPE zmmt0042-data_atual,
         xblnr      TYPE zmmt0042-xblnr,
       END OF ty_zmmt0042,

       BEGIN OF ty_rbkp,
         belnr TYPE rbkp-belnr,
         stblg TYPE rbkp-stblg,
         budat TYPE rbkp-budat,
         usnam TYPE rbkp-usnam,
       END OF ty_rbkp,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w,

       BEGIN OF ty_user_addr,
         bname      TYPE user_addr-bname,
         name_textc TYPE user_addr-name_textc,
       END OF ty_user_addr,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_t161t,
         bsart TYPE t161t-bsart,
         batxt TYPE t161t-batxt,
       END OF ty_t161t,

       BEGIN OF ty_esll,
         packno     TYPE esll-packno,
         sub_packno TYPE esll-sub_packno,
         srvpos     TYPE esll-srvpos,
       END OF ty_esll,

       BEGIN OF ty_asmdt,
         asnum TYPE asmdt-asnum,
         asktx TYPE asmdt-asktx,
       END OF ty_asmdt,


       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
       END OF ty_mara,

       BEGIN OF ty_saida,
         ekgrp                  TYPE ekko-ekgrp,
         eknam                  TYPE t024-eknam,
         name1                  TYPE t001w-name1,
         werks                  TYPE eban-werks,
         lgort                  TYPE eban-lgort,
         cod_material           TYPE esll-srvpos,
         texto_material         TYPE asmdt-asktx,
         badat                  TYPE eban-badat,
         usuario_cria_req       TYPE user_addr-name_textc,
         data_apro_req          TYPE eban-frgdt,
         hora_apro_req          TYPE zde_hor_apr_req,                     " 38883 - Sara em 14.07.2020
         usuario_aprov_req(100)," TYPE T16FC-FRGCO,
         tempo_aprov_req        TYPE i, "   TYPE EBAN-BADAT,
         ebeln                  TYPE ekko-ebeln,
         xblnr                  TYPE zmmt0042-xblnr,
         ebelp                  TYPE ekpo-ebelp,
         knttp                  TYPE ekpo-knttp,
         pstyp                  TYPE ekpo-pstyp,
         banfn                  TYPE eban-banfn,
         bnfpo                  TYPE eban-bnfpo,
         lfdat                  TYPE eban-lfdat,
         bsart(50),"             TYPE EBAN-BSART,
         aedat                  TYPE ekko-aedat,
         eindt                  TYPE eket-eindt,
         usuario_cria_ped       TYPE user_addr-name_textc,
         udate                  TYPE cdhdr-udate,
         usuario_aprov_ped      TYPE user_addr-name_textc,
         previsao               TYPE i, " TYPE CDHDR-UDATE,
         fornecedor(100),"       TYPE LFA1-NAME1,
         tempo_apr_ped          TYPE i, "     TYPE EKKO-AEDAT,
         tempo_aten_comp        TYPE i, "  TYPE EBAN-BADAT,
         data_rec_fisico        TYPE ekbe-budat,
         usuario_rec_fisico     TYPE user_addr-name_textc,
         data_rem_receb         TYPE i, "         TYPE EKET-EINDT,
         tempo_atend_req        TYPE i, "        TYPE EBAN-BADAT,
         data_criacao_apr       TYPE zmmt0042-data_atual,
         apr_rec_fisico         TYPE i, "        TYPE ZMMT0042-DATA_ATUAL,
         apr_rec_fiscal         TYPE i, "         TYPE ZMMT0042-DATA_ATUAL,
         data_rec_fiscal        TYPE rbkp-budat,
         usuario_rec_fiscal     TYPE user_addr-name_textc,
         rec_fiscal_fisico      TYPE i, "  TYPE EKBE-BUDAT,
         apr_req_migo           TYPE i,
         cod_servico(100),
         konnr_rc               TYPE eban-konnr,
         konnr_pc               TYPE ekpo-konnr,
         prio_urg               TYPE eban-prio_urg,
         prio_urgtx             TYPE purgtx_t-prio_urgtx,
         mtart                  TYPE mara-mtart,

       END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: t_eban     TYPE TABLE OF ty_eban,
      t_t16fc    TYPE TABLE OF ty_t16fc,
      t_t16fd    TYPE TABLE OF ty_t16fd,
      t_ekpo     TYPE TABLE OF ty_ekpo,
      t_esll     TYPE TABLE OF ty_esll,
      t_esll_aux TYPE TABLE OF ty_esll,
      t_asmdt    TYPE TABLE OF ty_asmdt,
      t_ekko     TYPE TABLE OF ty_ekko,
      t_t024     TYPE TABLE OF ty_t024,
      t_t161t    TYPE TABLE OF ty_t161t,
      t_cdhdr    TYPE TABLE OF ty_cdhdr,
      t_cdpos    TYPE TABLE OF ty_cdpos,
      t_eket     TYPE TABLE OF ty_eket,
      t_ekbe     TYPE TABLE OF ty_ekbe,
      t_ekbe_aux TYPE TABLE OF ty_ekbe,
      t_mseg_est TYPE TABLE OF mseg,
      t_mseg     TYPE TABLE OF ty_mseg,
      t_0042     TYPE TABLE OF ty_zmmt0042,
      t_rbkp     TYPE TABLE OF ty_rbkp,
      t_t001w    TYPE TABLE OF ty_t001w,
      t_addr     TYPE TABLE OF ty_user_addr,
      t_lfa1     TYPE TABLE OF ty_lfa1,
      t_saida    TYPE TABLE OF ty_saida,
      t_zmmt0099 TYPE TABLE OF zmmt0099,
      t_purgtx_t TYPE TABLE OF purgtx_t,
      t_mara     TYPE TABLE OF ty_mara.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_eban     TYPE ty_eban,
      wa_t16fc    TYPE ty_t16fc,
      wa_t16fd    TYPE ty_t16fd,
      wa_ekpo     TYPE ty_ekpo,
      wa_esll     TYPE ty_esll,
      wa_asmdt    TYPE ty_asmdt,
      wa_ekko     TYPE ty_ekko,
      wa_t024     TYPE ty_t024,
      wa_t161t    TYPE ty_t161t,
      wa_cdhdr    TYPE ty_cdhdr,
      wa_cdpos    TYPE ty_cdpos,
      wa_eket     TYPE ty_eket,
      wa_ekbe     TYPE ty_ekbe,
      wa_ekbe_aux TYPE ty_ekbe,
      wa_mseg_est TYPE mseg,
      wa_mseg     TYPE ty_mseg,
      wa_0042     TYPE ty_zmmt0042,
      wa_rbkp     TYPE ty_rbkp,
      wa_t001w    TYPE ty_t001w,
      wa_addr     TYPE ty_user_addr,
      wa_lfa1     TYPE ty_lfa1,
      wa_saida    TYPE ty_saida,
      wa_zmmt0099 TYPE zmmt0099,
      wa_purgtx_t TYPE purgtx_t,
      wa_mara     TYPE ty_mara.

*----------------------------------------------------------------------*
* VARIÁVEIS
*----------------------------------------------------------------------*
*  DATA: WG_EBELN TYPE CDHDR-OBJECTID.

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
      lt_sort      TYPE slis_t_sortinfo_alv,
      ls_sort      TYPE slis_sortinfo_alv.

DATA: gs_variant_c TYPE disvariant.

DATA: ans  TYPE c,
      conf TYPE c.

DATA: l_erro TYPE c.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR ekko-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_werks FOR wa_eban-werks OBLIGATORY,
                s_badat FOR wa_eban-badat OBLIGATORY,
                s_banfn FOR wa_eban-banfn,
                s_lgort FOR wa_eban-lgort,
                s_bsart FOR wa_ekko-bsart.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
PARAMETERS:
  r_st_r RADIOBUTTON GROUP rad1 USER-COMMAND act DEFAULT 'X',
  r_ap_r RADIOBUTTON GROUP rad1,
  r_cr_p RADIOBUTTON GROUP rad1,
*  R_AP_P RADIOBUTTON GROUP RAD1,
  r_st_m RADIOBUTTON GROUP rad1,
  r_mi_m RADIOBUTTON GROUP rad1.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
PARAMETERS: r_ex_bo AS CHECKBOX DEFAULT ' ' USER-COMMAND verificacao.
SELECTION-SCREEN: END OF BLOCK b3.



AT SELECTION-SCREEN ON r_ex_bo.

  IF r_ex_bo IS NOT INITIAL.

    CHECK conf IS INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar      = 'Confirmação'
        text_question = 'Deseja realizar o processo?'
        text_button_1 = 'Sim'
        icon_button_1 = 'ICON_CHECKED'
        text_button_2 = 'Não'
        icon_button_2 = 'ICON_CANCEL'
      IMPORTING
        answer        = ans.

    CASE ans.
      WHEN '2' OR 'A'.
        conf = abap_false.
        CLEAR r_ex_bo.
      WHEN '1'.
        conf = abap_true.
    ENDCASE.
  ENDIF.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR l_erro.
  PERFORM zf_validar_deposito CHANGING l_erro.

  IF l_erro IS INITIAL.
    PERFORM selecionar_dados.
    PERFORM organizar_dados.
    PERFORM iniciar_variaveis.
    PERFORM imprimir_dados.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .
  DATA:wl_index TYPE sy-tabix.

  IF r_st_r = 'X'. "Requisição
    SELECT matnr werks badat banfn bnfpo ernam frgdt frgst txz01 packno pstyp knttp lfdat lgort banfn  konnr prio_urg
      FROM eban
        INTO TABLE t_eban
          WHERE werks IN s_werks
            AND badat IN s_badat
            AND banfn IN s_banfn
            AND lgort IN s_lgort.

  ELSEIF r_ap_r = 'X'. "Aprovação de Requisição
    SELECT matnr werks badat banfn bnfpo ernam frgdt frgst txz01 packno pstyp knttp lfdat  lgort  banfn konnr  prio_urg
      FROM eban
        INTO TABLE t_eban
          WHERE werks IN s_werks
            AND frgdt IN s_badat
            AND banfn IN s_banfn
                  AND lgort IN s_lgort.

  ELSEIF r_cr_p = 'X'. "Criação do Pedido

    SELECT eban~matnr eban~werks eban~badat eban~banfn eban~bnfpo eban~ernam eban~frgdt
      eban~frgst eban~txz01 eban~packno eban~pstyp eban~knttp eban~lfdat eban~lgort eban~banfn eban~konnr eban~prio_urg
    FROM eban
      INNER JOIN ekpo
      ON  ekpo~banfn EQ eban~banfn
      AND ekpo~bnfpo EQ eban~bnfpo
      INNER JOIN ekko
      ON  ekko~ebeln  EQ ekpo~ebeln
      INTO TABLE t_eban
        WHERE ekko~aedat IN s_badat.

    SORT t_eban BY matnr werks badat banfn bnfpo ernam frgdt frgst txz01 packno pstyp banfn konnr prio_urg.
    DELETE ADJACENT DUPLICATES FROM t_eban COMPARING ALL FIELDS.

  ELSEIF r_st_m = 'X'. "Migo

    SELECT eban~matnr eban~werks eban~badat eban~banfn eban~bnfpo eban~ernam eban~frgdt eban~frgst eban~txz01
      eban~packno eban~pstyp eban~knttp eban~lfdat  eban~lgort eban~banfn eban~konnr eban~prio_urg
     FROM  ekbe AS eb
     INNER JOIN ekpo AS ek
     ON  ek~ebeln EQ eb~ebeln
     AND ek~ebelp EQ eb~ebelp
     INNER JOIN eban
     ON  eban~banfn EQ ek~banfn
     AND eban~bnfpo EQ ek~bnfpo
     INTO TABLE t_eban
     WHERE ek~bukrs   IN s_bukrs
       AND eb~budat   IN s_badat
       AND eb~bewtp   EQ 'E'
       AND eban~werks IN s_werks
       AND eban~banfn IN s_banfn
       AND  eban~lgort IN s_lgort.

    SORT t_eban BY matnr werks badat banfn bnfpo ernam frgdt frgst txz01 packno pstyp knttp.
    DELETE ADJACENT DUPLICATES FROM t_eban COMPARING ALL FIELDS.

  ELSEIF r_mi_m = 'X'. "Miro

    SELECT eban~matnr eban~werks eban~badat eban~banfn eban~bnfpo eban~ernam eban~frgdt eban~frgst
      eban~txz01 eban~packno eban~pstyp eban~knttp eban~lfdat eban~lgort  eban~banfn eban~konnr eban~prio_urg
     FROM  ekbe AS eb
     INNER JOIN ekpo AS ek
     ON  ek~ebeln EQ eb~ebeln
     AND ek~ebelp EQ eb~ebelp
     INNER JOIN eban
     ON  eban~banfn EQ ek~banfn
     AND eban~bnfpo EQ ek~bnfpo
     INTO TABLE t_eban
     WHERE ek~bukrs   IN s_bukrs
       AND eb~budat   IN s_badat
       AND eb~bewtp   EQ 'Q'
       AND eban~werks IN s_werks
       AND eban~banfn IN s_banfn
       AND  eban~lgort IN s_lgort.

    SORT t_eban BY matnr werks badat banfn bnfpo ernam frgdt frgst txz01 packno pstyp.
    DELETE ADJACENT DUPLICATES FROM t_eban COMPARING ALL FIELDS.

  ENDIF.

  IF t_eban IS NOT INITIAL.
    "REQUISICAO
*    SELECT OBJECTCLAS OBJECTID CHANGENR TCODE USERNAME UDATE
    SELECT objectclas objectid changenr tcode username udate utime               " 38883 - Sara em 14.07.2020
     FROM cdhdr
       INTO TABLE t_cdhdr
       FOR ALL ENTRIES IN t_eban
          WHERE objectid   EQ t_eban-objectid
            AND objectclas EQ 'BANF'
            AND ( tcode EQ 'ME54N' OR tcode EQ 'ME55' OR  tcode EQ '' ).

    IF t_cdhdr[] IS NOT INITIAL.
      SELECT  objectclas objectid changenr fname value_new tabkey
       FROM cdpos
       INTO TABLE t_cdpos
       FOR ALL ENTRIES IN t_cdhdr
       WHERE objectclas EQ t_cdhdr-objectclas
       AND   objectid   EQ t_cdhdr-objectid
       AND   changenr   EQ t_cdhdr-changenr
       AND   fname      EQ 'FRGZU'
       AND   value_new  EQ 'X'.

      SELECT bname name_textc
        FROM user_addr
          INTO TABLE t_addr
          FOR ALL ENTRIES IN t_cdhdr
            WHERE bname = t_cdhdr-username.
    ENDIF.


    SELECT packno sub_packno srvpos
      FROM esll
        INTO TABLE t_esll
        FOR ALL ENTRIES IN t_eban
            WHERE packno EQ t_eban-packno.

    t_esll_aux[] = t_esll[].

    IF t_esll_aux[] IS NOT INITIAL.
      SELECT packno sub_packno srvpos
         FROM esll
           APPENDING TABLE t_esll
           FOR ALL ENTRIES IN t_esll_aux
             WHERE packno EQ t_esll_aux-sub_packno.

      IF sy-subrc IS INITIAL.
        SELECT asnum asktx
           FROM asmdt
             INTO TABLE t_asmdt
             FOR ALL ENTRIES IN t_esll
               WHERE asnum EQ t_esll-srvpos
                 AND spras EQ sy-langu.

      ENDIF.
    ENDIF.

    SELECT frgco frgct
      FROM t16fd
        INTO TABLE t_t16fd
        FOR ALL ENTRIES IN t_eban
          WHERE frgco EQ t_eban-frgst
          AND   frggr = '02'
            AND spras EQ sy-langu.

*    SELECT BNAME NAME_TEXTC
*      FROM USER_ADDR
*        INTO TABLE T_ADDR
*        FOR ALL ENTRIES IN T_EBAN
*          WHERE BNAME = T_EBAN-ERNAM.

    SELECT *
        FROM purgtx_t
        INTO TABLE t_purgtx_t
          FOR ALL ENTRIES IN t_eban
            WHERE spras EQ sy-langu
            AND   prio_urg EQ t_eban-prio_urg.


    SELECT matnr
           mtart
      FROM mara
       INTO TABLE t_mara
      FOR ALL ENTRIES IN t_eban
     WHERE matnr EQ t_eban-matnr.


    SELECT  banfn bnfpo ebeln ebelp matnr txz01 werks ebeln pstyp knttp packno konnr
      FROM ekpo
        INTO TABLE t_ekpo
        FOR ALL ENTRIES IN t_eban
          WHERE banfn EQ t_eban-banfn
            AND bnfpo EQ t_eban-bnfpo
            AND loekz EQ ''.

    IF sy-subrc IS INITIAL.
      SELECT ebeln bsart aedat ernam lifnr ekgrp
         FROM ekko
           INTO TABLE t_ekko
           FOR ALL ENTRIES IN t_ekpo
             WHERE ebeln EQ t_ekpo-ebeln
               AND bsart IN s_bsart.

      IF sy-subrc IS INITIAL.
        SELECT ekgrp eknam
          FROM t024
            INTO TABLE t_t024
            FOR ALL ENTRIES IN t_ekko
              WHERE ekgrp EQ t_ekko-ekgrp.

        SELECT bsart batxt
          FROM t161t
            INTO TABLE t_t161t
            FOR ALL ENTRIES IN t_ekko
              WHERE bsart EQ t_ekko-bsart
                AND spras EQ sy-langu.

        SELECT lifnr name1
          FROM lfa1
            INTO TABLE t_lfa1
            FOR ALL ENTRIES IN t_ekko
              WHERE lifnr EQ t_ekko-lifnr.

        SELECT ebeln ebelp eindt
          FROM eket
            INTO TABLE t_eket
            FOR ALL ENTRIES IN t_ekko
              WHERE ebeln EQ t_ekko-ebeln.


        SELECT bname name_textc
          FROM user_addr
            APPENDING TABLE t_addr
            FOR ALL ENTRIES IN t_ekko
              WHERE bname = t_ekko-ernam.
      ENDIF.

      "PEDIDO
      SELECT objectclas objectid changenr tcode username udate
        FROM cdhdr
          APPENDING TABLE t_cdhdr
          FOR ALL ENTRIES IN t_ekpo
             WHERE objectid   EQ t_ekpo-objectid
               AND objectclas EQ 'EINKBELEG'
               AND ( tcode EQ 'ME29N' OR  tcode EQ '' ).

      IF t_cdhdr[] IS NOT INITIAL.
        SELECT  objectclas objectid changenr fname value_new tabkey
          FROM cdpos
          APPENDING TABLE t_cdpos
          FOR ALL ENTRIES IN t_cdhdr
          WHERE objectclas EQ t_cdhdr-objectclas
          AND   objectid   EQ t_cdhdr-objectid
          AND   changenr   EQ t_cdhdr-changenr
          AND   fname      EQ 'FRGKE'
          AND   value_new  EQ '2'.


        SELECT bname name_textc
          FROM user_addr
            APPENDING TABLE t_addr
            FOR ALL ENTRIES IN t_cdhdr
              WHERE bname = t_cdhdr-username.

      ENDIF.

      SELECT ebeln ebelp data_atual xblnr
        FROM zmmt0042
          INTO TABLE t_0042
          FOR ALL ENTRIES IN t_ekpo
            WHERE ebeln EQ t_ekpo-ebeln
              AND ebelp EQ t_ekpo-ebelp.

      SELECT werks name1
        FROM t001w
          INTO TABLE t_t001w
          FOR ALL ENTRIES IN t_ekpo
            WHERE werks EQ t_ekpo-werks.

      IF r_st_r = 'X'.
        SELECT ebeln ebelp vgabe shkzg belnr budat ernam
           FROM ekbe
             INTO TABLE t_ekbe
             FOR ALL ENTRIES IN t_ekpo
               WHERE ebeln EQ t_ekpo-ebeln
                 AND ebelp EQ t_ekpo-ebelp
                 AND shkzg EQ 'S'
                 AND (    vgabe EQ '1'
                       OR vgabe EQ '2' ).
      ELSE.
        SELECT ebeln ebelp vgabe shkzg belnr budat ernam
           FROM ekbe
             INTO TABLE t_ekbe
             FOR ALL ENTRIES IN t_ekpo
               WHERE ebeln EQ t_ekpo-ebeln
                 AND ebelp EQ t_ekpo-ebelp
                 AND budat IN s_badat
                 AND shkzg EQ 'S'
                 AND (    vgabe EQ '1'
                       OR vgabe EQ '2' ).
      ENDIF.

      IF sy-subrc IS INITIAL.
        t_ekbe_aux[] = t_ekbe[].
        DELETE t_ekbe_aux WHERE vgabe NE '1'.

        IF t_ekbe_aux[] IS NOT INITIAL.
          SELECT *
            FROM mseg
              INTO TABLE t_mseg_est
              FOR ALL ENTRIES IN t_ekbe_aux
                WHERE smbln EQ t_ekbe_aux-belnr.

          LOOP AT t_ekbe INTO wa_ekbe
            WHERE vgabe EQ '1'.

            READ TABLE t_mseg_est INTO wa_mseg_est
              WITH KEY smbln = wa_ekbe-belnr
                       ebeln = wa_ekbe-ebeln
                       ebelp = wa_ekbe-ebelp.

            IF sy-subrc IS INITIAL.
              DELETE t_ekbe.
            ENDIF.

          ENDLOOP.

          SELECT bname name_textc
            FROM user_addr
              APPENDING TABLE t_addr
              FOR ALL ENTRIES IN t_ekbe_aux
                WHERE bname EQ t_ekbe_aux-ernam.
        ENDIF.

        t_ekbe_aux[] = t_ekbe[].
        DELETE t_ekbe_aux WHERE vgabe NE '2'.

        IF t_ekbe_aux[] IS NOT INITIAL.
          SELECT belnr stblg budat usnam
            FROM rbkp
              INTO TABLE t_rbkp
              FOR ALL ENTRIES IN t_ekbe_aux
                WHERE belnr EQ t_ekbe_aux-belnr.
*                  AND STBLG EQ SPACE.

          LOOP AT t_ekbe INTO wa_ekbe
            WHERE vgabe EQ '2'.

            READ TABLE t_rbkp INTO wa_rbkp
              WITH KEY belnr = wa_ekbe-belnr.

            IF sy-subrc IS INITIAL.
              wl_index = sy-tabix.

              IF wa_rbkp-stblg NE space.
                DELETE t_rbkp INDEX wl_index.
                DELETE t_ekbe WHERE belnr EQ wa_ekbe-belnr.
              ENDIF.
            ENDIF.


          ENDLOOP.

          IF sy-subrc IS INITIAL.
            SELECT bname name_textc
              FROM user_addr
                APPENDING TABLE t_addr
                FOR ALL ENTRIES IN t_rbkp
                  WHERE bname = t_rbkp-usnam.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizar_dados .
  DATA: wl_dias(10) TYPE c,
        wl_index    TYPE sy-tabix,
        wl_budat    TYPE ekbe-budat,
        vtabkey     TYPE cdpos-tabkey,
        xachou(1).

  SORT: t_t024 BY ekgrp,
        t_eket BY ebeln ebelp,
        t_cdhdr BY objectclas objectid  ASCENDING changenr DESCENDING,
        t_cdpos BY objectclas objectid changenr tabkey.

  CLEAR: wa_saida, wa_t16fc, wa_ekpo, wa_ekko, wa_cdhdr, wa_eket, wa_esll, wa_asmdt,
         wa_ekbe, wa_mseg, wa_0042, wa_rbkp, wa_t001w, wa_addr, wa_lfa1, wa_t16fd, wa_purgtx_t, wa_mara.

  LOOP AT t_eban INTO wa_eban.

    CLEAR: wa_saida.

    IF wa_eban-pstyp EQ '9'.
      READ TABLE t_esll INTO wa_esll
        WITH KEY packno = wa_eban-packno.

      IF sy-subrc IS INITIAL.
        READ TABLE t_esll INTO wa_esll
          WITH KEY packno = wa_esll-sub_packno.

        IF sy-subrc IS INITIAL.
          READ TABLE t_asmdt INTO wa_asmdt
            WITH KEY asnum = wa_esll-srvpos.

          wa_saida-cod_material   = wa_esll-srvpos.
          wa_saida-texto_material = wa_asmdt-asktx.
*          CONCATENATE WA_ESLL-SRVPOS '-' WA_ASMDT-ASKTX INTO WA_SAIDA-COD_SERVICO SEPARATED BY SPACE.
        ENDIF.
      ENDIF.

    ELSE.
*---> 14/06/2023 - Migração S4 - JS
*       wa_saida-cod_material   = wa_eban-matnr.
      wa_saida-cod_material = CONV #( wa_eban-matnr ).
*<--- 14/06/2023 - Migração S4 - JS
      wa_saida-texto_material = wa_eban-txz01.
    ENDIF.

    READ TABLE t_mara INTO wa_mara
      WITH KEY matnr = wa_eban-matnr.
    IF sy-subrc EQ 0.
      wa_saida-mtart = wa_mara-mtart.
    ENDIF.

    READ TABLE t_t16fd INTO wa_t16fd
      WITH KEY frgco = wa_eban-frgst.

    READ TABLE t_addr INTO wa_addr
      WITH KEY bname = wa_eban-ernam.

    IF sy-subrc IS INITIAL.
      wa_saida-usuario_cria_req = wa_addr-name_textc.
    ELSE.
      wa_addr-name_textc        = wa_eban-ernam.
      wa_saida-usuario_cria_req = wa_addr-name_textc.
    ENDIF.

    READ TABLE t_t001w INTO wa_t001w
      WITH KEY werks = wa_eban-werks.

    READ TABLE t_ekpo INTO wa_ekpo
      WITH KEY banfn = wa_eban-banfn
               bnfpo = wa_eban-bnfpo.

    IF sy-subrc IS INITIAL.
      READ TABLE t_0042 INTO wa_0042
        WITH KEY ebeln = wa_ekpo-ebeln
                 ebelp = wa_ekpo-ebelp.

      READ TABLE t_ekko INTO wa_ekko
        WITH KEY ebeln = wa_ekpo-ebeln.

      IF sy-subrc IS INITIAL.
        READ TABLE t_t024 INTO wa_t024
          WITH KEY ekgrp = wa_ekko-ekgrp.

        IF sy-subrc EQ 0.
          wa_saida-ekgrp = wa_t024-ekgrp.
          wa_saida-eknam = wa_t024-eknam.
        ENDIF.

        READ TABLE t_t161t INTO wa_t161t
          WITH KEY bsart = wa_ekko-bsart.

        READ TABLE t_lfa1 INTO wa_lfa1
          WITH KEY lifnr = wa_ekko-lifnr.

** EJ
        READ TABLE t_addr INTO wa_addr
          WITH KEY bname = wa_ekko-ernam.

        IF sy-subrc IS INITIAL.
          wa_saida-usuario_cria_ped = wa_addr-name_textc.
        ELSE.
          wa_addr-name_textc        = wa_ekko-ernam.
          wa_saida-usuario_cria_ped = wa_addr-name_textc.
        ENDIF.
**

*       Tempo atendimento comprador
        PERFORM calcula_dias USING wa_ekko-aedat
                                   wa_eban-badat
                             CHANGING wl_dias.

        wa_saida-tempo_aten_comp = wl_dias.

*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-TEMPO_ATEN_COMP SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-TEMPO_ATEN_COMP SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

        CONCATENATE wa_ekko-lifnr '-' wa_lfa1-name1
               INTO wa_saida-fornecedor SEPARATED BY space.


        CLEAR wa_saida-eindt.
        READ TABLE t_eket INTO wa_eket
          WITH KEY ebeln = wa_ekpo-ebeln
                   ebelp = wa_ekpo-ebelp BINARY SEARCH.

        IF sy-subrc = 0.
          wa_saida-eindt = wa_eket-eindt.
        ENDIF.

        wa_saida-xblnr            = wa_0042-xblnr.
        wa_saida-data_criacao_apr = wa_0042-data_atual.
      ENDIF.

      READ TABLE t_ekbe INTO wa_ekbe
        WITH KEY ebeln = wa_ekpo-ebeln
                 ebelp = wa_ekpo-ebelp
                 vgabe = '1'
                 shkzg = 'S'.

      wl_budat = wa_ekbe-budat.
      IF sy-subrc IS INITIAL.

        READ TABLE t_addr INTO wa_addr
          WITH KEY bname = wa_ekbe-ernam.

        IF sy-subrc IS NOT INITIAL.
          wa_addr-name_textc = wa_ekbe-ernam.
        ENDIF.


*       Data remessa x recebimento
        PERFORM calcula_dias USING wa_ekbe-budat
                                   wa_eket-eindt
                             CHANGING wl_dias.

        wa_saida-data_rem_receb = wl_dias.
*
*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-DATA_REM_RECEB SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-DATA_REM_RECEB SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

**>      Tempo atendimento real da requisição
        PERFORM calcula_dias USING wa_ekbe-budat
                                   wa_eban-badat
                             CHANGING wl_dias.

        wa_saida-tempo_atend_req = wl_dias.

*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-TEMPO_ATEND_REQ SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-TEMPO_ATEND_REQ SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.


**> APR x recebimento físico
        PERFORM calcula_dias USING wa_ekbe-budat
                                   wa_0042-data_atual
                             CHANGING wl_dias.

        wa_saida-apr_rec_fisico = wl_dias.
*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-APR_REC_FISICO SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-APR_REC_FISICO SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

**> Aprov. Requisição X MIGO
        PERFORM calcula_dias USING wa_ekbe-budat
                                   wa_eban-frgdt
                             CHANGING wl_dias.

        wa_saida-apr_req_migo = wl_dias.
*
*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-APR_REQ_MIGO SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-APR_REQ_MIGO SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

        wa_saida-data_rec_fisico    = wa_ekbe-budat.
        wa_saida-usuario_rec_fisico = wa_addr-name_textc.
        wa_saida-data_rec_fisico    = wa_ekbe-budat.
        wa_saida-usuario_rec_fisico = wa_addr-name_textc.
      ENDIF.

      READ TABLE t_ekbe INTO wa_ekbe
         WITH KEY ebeln = wa_ekpo-ebeln
                  ebelp = wa_ekpo-ebelp
                  vgabe = '2'
                  shkzg = 'S'.

      IF sy-subrc IS INITIAL.
        READ TABLE t_rbkp INTO wa_rbkp
          WITH KEY belnr = wa_ekbe-belnr
                   stblg = space.


        IF sy-subrc IS INITIAL.
          READ TABLE t_addr INTO wa_addr
            WITH KEY bname = wa_rbkp-usnam.

          IF sy-subrc IS NOT INITIAL.
            wa_addr-name_textc = wa_rbkp-usnam.
          ENDIF.

**> APR x recebimento fiscal
          PERFORM calcula_dias USING wa_rbkp-budat
                                     wa_0042-data_atual
                               CHANGING wl_dias.

          wa_saida-apr_rec_fiscal = wl_dias.

*          IF WL_DIAS EQ '0'
*          OR WL_DIAS EQ '1'.
*            CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-APR_REC_FISCAL SEPARATED BY SPACE .
*            CLEAR: WL_DIAS.
*          ELSE.
*            CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-APR_REC_FISCAL SEPARATED BY SPACE .
*            CLEAR: WL_DIAS.
*          ENDIF.

**> Recebimento fiscal x físico
          PERFORM calcula_dias USING  wa_rbkp-budat
                                      wl_budat
                                CHANGING wl_dias.

          wa_saida-rec_fiscal_fisico = wl_dias.

*         IF WL_DIAS EQ '0'
*           OR WL_DIAS EQ '1'.
*            CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-REC_FISCAL_FISICO SEPARATED BY SPACE .
*            CLEAR: WL_DIAS.
*          ELSE.
*            CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-REC_FISCAL_FISICO SEPARATED BY SPACE .
*            CLEAR: WL_DIAS.
*          ENDIF.

          wa_saida-data_rec_fiscal    = wa_rbkp-budat.
          wa_saida-usuario_rec_fiscal = wa_addr-name_textc.
        ENDIF.
      ENDIF.

      CLEAR xachou.
      LOOP AT t_cdhdr INTO wa_cdhdr
                       WHERE objectclas = 'EINKBELEG'
                       AND   objectid   = wa_ekpo-objectid.
        READ TABLE t_cdpos INTO wa_cdpos WITH KEY  objectclas = wa_cdhdr-objectclas
                                                   objectid   = wa_cdhdr-objectid
                                                   changenr   = wa_cdhdr-changenr BINARY SEARCH.
        IF sy-subrc = 0.
          xachou = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF xachou = 'X'.
        READ TABLE t_addr INTO wa_addr
          WITH KEY bname = wa_cdhdr-username.

        IF sy-subrc IS NOT INITIAL.
          wa_addr-name_textc = wa_cdhdr-username.
        ENDIF.

**> PREVISÃO ENTREGA MATERIAL
        PERFORM calcula_dias USING  wa_eket-eindt
                                    wa_cdhdr-udate
                              CHANGING wl_dias.

        wa_saida-previsao = wl_dias.

*        IF WL_DIAS EQ '0'
*        OR WL_DIAS EQ '1'.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-PREVISAO SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-PREVISAO  SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

**> TEMPO APROVAÇÃO DO PEDIDO
        PERFORM calcula_dias USING  wa_cdhdr-udate
                                    wa_ekko-aedat
                             CHANGING wl_dias.

        wa_saida-tempo_apr_ped = wl_dias.

*        IF WL_DIAS EQ 0
*        OR WL_DIAS EQ 1.
*          CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-TEMPO_APR_PED SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ELSE.
*          CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-TEMPO_APR_PED SEPARATED BY SPACE .
*          CLEAR: WL_DIAS.
*        ENDIF.

        wa_saida-usuario_aprov_ped = wa_addr-name_textc.
      ENDIF.


      wa_saida-ebeln   = wa_ekko-ebeln.
      wa_saida-bsart   = wa_t161t-batxt.
      wa_saida-aedat   = wa_ekko-aedat.
    ENDIF.

    wa_saida-data_criacao_apr = wa_0042-data_atual.
    wa_saida-udate            = wa_cdhdr-udate.
*  ENDIF.


*    IF WL_DIAS EQ '0'
*    OR WL_DIAS EQ '1'.
*      CONCATENATE  WL_DIAS 'Dia' INTO WA_SAIDA-TEMPO_APROV_REQ SEPARATED BY SPACE .
*      CLEAR: WL_DIAS.
*    ELSE.
*      CONCATENATE  WL_DIAS 'Dias' INTO WA_SAIDA-TEMPO_APROV_REQ SEPARATED BY SPACE .
*      CLEAR: WL_DIAS.
*    ENDIF.


    wa_saida-usuario_aprov_req  = wa_t16fd-frgct.
    wa_saida-ebelp              = wa_ekpo-ebelp.
    wa_saida-knttp              = wa_ekpo-knttp.
    wa_saida-pstyp              = wa_ekpo-pstyp.
    wa_saida-konnr_pc           = wa_ekpo-konnr.
    wa_saida-werks              = wa_eban-werks.
    wa_saida-name1              = wa_t001w-name1.
    wa_saida-banfn              = wa_eban-banfn.
    wa_saida-bnfpo              = wa_eban-bnfpo.
    wa_saida-lfdat              = wa_eban-lfdat.
    wa_saida-badat              = wa_eban-badat.
    wa_saida-konnr_rc           = wa_eban-konnr.
    wa_saida-prio_urg           = wa_eban-prio_urg.

    READ TABLE t_purgtx_t INTO wa_purgtx_t WITH KEY prio_urg = wa_eban-prio_urg.
    IF sy-subrc EQ 0.
      wa_saida-prio_urgtx = wa_purgtx_t-prio_urgtx.
    ENDIF.

    "alrs
    CLEAR xachou.
    CONCATENATE sy-mandt wa_eban-banfn wa_eban-bnfpo INTO vtabkey.
    LOOP AT t_cdhdr INTO wa_cdhdr
                     WHERE objectclas = 'BANF'
                     AND   objectid   = wa_eban-objectid.
      READ TABLE t_cdpos INTO wa_cdpos WITH KEY  objectclas = wa_cdhdr-objectclas
                                                 objectid   = wa_cdhdr-objectid
                                                 changenr   = wa_cdhdr-changenr
                                                 tabkey     = vtabkey BINARY SEARCH.
      IF sy-subrc = 0.
        xachou = 'X'.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF xachou = 'X'.
      READ TABLE t_addr INTO wa_addr
        WITH KEY bname = wa_cdhdr-username.

      IF sy-subrc IS INITIAL.
        wa_saida-usuario_aprov_req = wa_addr-name_textc.
      ELSEIF wa_saida-usuario_aprov_req IS INITIAL.
        wa_saida-usuario_aprov_req = wa_cdhdr-username.
      ENDIF.

      wa_saida-data_apro_req     = wa_cdhdr-udate.
      wa_saida-hora_apro_req     = wa_cdhdr-utime.                " 38883 - Sara em 14.07.2020
    ENDIF.

**> CALCULO DE DIAS
    PERFORM calcula_dias USING  wa_saida-data_apro_req
                                wa_eban-badat
                        CHANGING wl_dias.

    wa_saida-tempo_aprov_req = wl_dias.

    CLEAR wa_saida-lgort.
    wa_saida-lgort = wa_eban-lgort.

    APPEND wa_saida TO t_saida.
    CLEAR: wa_saida, wa_t16fc, wa_ekpo, wa_ekko, wa_cdhdr, wa_eket, wa_esll, wa_asmdt,
           wa_ekbe, wa_mseg, wa_0042, wa_rbkp, wa_t001w, wa_addr, wa_lfa1, wa_t16fd, wa_mara.

  ENDLOOP.
ENDFORM.                    " ORGANIZAR_DADOS

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .
  DATA: wl_layout TYPE slis_layout_alv.

  PERFORM z_export_sap_bo.

  PERFORM definir_eventos.
  PERFORM montar_layout.

* Ordenar a saida do alv
  ADD 1 TO ls_sort-spos.
  ls_sort-fieldname = 'EBELN'.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  APPEND ls_sort TO lt_sort.
*
*  CLEAR: LS_SORT-SPOS.
*  ADD 1 TO LS_SORT-SPOS.
*  LS_SORT-FIELDNAME = 'EBELP'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = 'X'.
*  APPEND LS_SORT TO LT_SORT.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_variant         = gs_variant_c
*     I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
      it_fieldcat        = estrutura[]
      is_layout          = wl_layout
      i_save             = 'X'
      it_events          = events
      is_print           = t_print
      it_sort            = lt_sort
    TABLES
      t_outtab           = t_saida.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.

  PERFORM f_carregar_eventos USING:
*                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
*                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.

  PERFORM montar_estrutura USING:
        1  'T024'              'EKGRP'          'T_SAIDA' 'EKGRP'              'Cód. Comprador'                             '15' ,
        1  'T024'              'EKNAM'          'T_SAIDA' 'EKNAM'              'Nome Comprador'                             '25' ,
        1  'T001W'             'NAME1'          'T_SAIDA' 'NAME1'              'Filial'                                     '20' ,
        2  'EBAN'              'WERKS'          'T_SAIDA' 'WERKS'              ' '                                          '6'  ,
        3  'EBAN'              'LGORT'          'T_SAIDA' 'LGORT'              'Depósito'                                   '10'  ,
*        3  'EBAN'              'MATNR'          'T_SAIDA' 'MATNR'              'Código material'                           '13' ,
        3  'MARA'              'MTART'          'T_SAIDA' 'MTART'              'Tipo de Material'                           '12',
        3  'ESLL'              'SRVPOS'         'T_SAIDA' 'COD_MATERIAL'       'Código Material ou Serviço'                 '26' ,
*        4  'EBAN'              'TXZ01'          'T_SAIDA' 'TXZ01'              'Texto breve material'                       '18' ,
        4  'ASMDT'             'ASKTX'          'T_SAIDA' 'TEXTO_MATERIAL'     'Texto Breve Material'                       '40' ,
        5  'EBAN '             'BADAT'          'T_SAIDA' 'BADAT'              'Data Criação Requisição'                    '19' ,
        6  'USER_ADDR'         'NAME_TEXTC'     'T_SAIDA' 'USUARIO_CRIA_REQ'   'Usuário Criador Requisição'                 '26' ,
        7  'EBAN'              'FRGDT'          'T_SAIDA' 'DATA_APRO_REQ'      'Data Aprovação Requisição'                  '25' ,
        8  ' '                 ' '              'T_SAIDA' 'USUARIO_APROV_REQ'  'Usuário aprovador requisição'               '23' ,
        9  ' '                 ' '              'T_SAIDA' 'TEMPO_APROV_REQ'    'Tempo Aprovação Requisição (Dias)'          '34' ,
       10  'EKKO'              'EBELN'          'T_SAIDA' 'EBELN'              'Pedido'                                     '10' ,
       11  ' '                 ' '              'T_SAIDA' 'XBLNR'              'APR'                                        '10' ,
       12  ' '                 ' '              'T_SAIDA' 'BSART'              'Tipo do Pedido'                             '15' ,
       13  'EKPO'              'EBELP'          'T_SAIDA' 'EBELP'              'Item Pedido de Compras'                     '22' ,
       10  'EKPO'              'KNTTP'          'T_SAIDA' 'KNTTP'              'Class.Ctb.'                                 '05' ,
       10  'EKPO'              'PSTYP'          'T_SAIDA' 'PSTYP'              'Ctg.Item'                                   '05' ,
       14  'EBAN'              'BANFN'          'T_SAIDA' 'BANFN'              'Requisição de Compra'                       '21' ,
       15  'EBAN'              'BNFPO'          'T_SAIDA' 'BNFPO'              'Item da Requisição de Compra'               '27' ,
       15  'EBAN'              ' '              'T_SAIDA' 'PRIO_URG'           'Urgência Necessidade'                       '20' ,
       15  ' '                 ' '              'T_SAIDA' 'PRIO_URGTX'         'Descrição Urg.Necess.'                      '20' ,
       15  'EBAN'              'LFDAT'          'T_SAIDA' 'LFDAT'              'Data Remessa RC'                            '10' ,
       14  'EKKO'              'AEDAT'          'T_SAIDA' 'AEDAT'              'Data Criação Pedido'                        '16' ,
       15  'EKET'              'EINDT'          'T_SAIDA' 'EINDT'              ' '                                          '14' ,
       15  'EBAN'              'KONNR'          'T_SAIDA' 'KONNR_RC'           'Contrato RC'                                '10' ,
       16  'USER_ADDR'         'NAME_TEXTC'     'T_SAIDA' 'USUARIO_CRIA_PED'   'Usuário Criador Pedido'                     '18' ,
       17  'CDHDR'             'UDATE'          'T_SAIDA' 'UDATE'              'Data Aprovação Pedido'                      '19' ,
       18  'USER_ADDR'         'NAME_TEXTC'     'T_SAIDA' 'USUARIO_APROV_PED'  'Usuário Aprovador Pedido'                   '21' ,
*       18  ' '                 ' '              'T_SAIDA' 'COD_SERVICO'        'Código do Serviço '                         '18' ,
*       18  'ASMDT'             'ASKTX'          'T_SAIDA' 'ASKTX'              ' '                                          '21' ,
       19  'EKPO'              'KONNR'          'T_SAIDA' 'KONNR_PC'           'Contrato PC'                                '10' ,
       19  ' '                 ' '              'T_SAIDA' 'PREVISAO'           'Data Remessa - Aprovação Pedido (Dias)'     '40' ,
       20  ' '                 ' '              'T_SAIDA' 'FORNECEDOR'         'Fornecedor'                                 '40' ,
       21  ' '                 ' '              'T_SAIDA' 'TEMPO_APR_PED'      'Tempo Aprovação Pedido (Dias)'              '26' ,
       22  ' '                 ' '              'T_SAIDA' 'TEMPO_ATEN_COMP'    'Tempo Atendimento Comprador (Dias)'         '35' ,
       23  ' '                 ' '              'T_SAIDA' 'DATA_REC_FISICO'    'Data Recebimento Físico'                    '19' ,
       24  'EKBE'              'BUDAT'          'T_SAIDA' 'APR_REQ_MIGO'       'Aprov. Requisição - Receb. Físico (Dias)'   '40' ,
       25  'USER_ADDR'         'NAME_TEXTC'     'T_SAIDA' 'USUARIO_REC_FISICO' 'Usuário Recebimento Físico'                 '26' ,
       26  ' '                 ' '              'T_SAIDA' 'DATA_REM_RECEB'     'Data remessa - Receb. Físico ( Dias )'      '37' ,
       27  ' '                 ' '              'T_SAIDA' 'TEMPO_ATEND_REQ'    'Tempo atend. Real da requisição ( Dias )'   '40' ,
       28  'ZMMT0042'          'DATA_ATUAL'     'T_SAIDA' 'DATA_CRIACAO_APR'   'Data criação APR'                           '14' ,
       29  ' '                 ' '              'T_SAIDA' 'APR_REC_FISICO'     'Recebimento Físico - APR (Dias)'            '31' ,
       30  ' '                 ' '              'T_SAIDA' 'APR_REC_FISCAL'     'Recebimento Fiscal - APR (Dias)'            '31' ,
       31  'RBKP'              'BUDAT'          'T_SAIDA' 'DATA_REC_FISCAL'    'Data Recebimento Fiscal'                    '23' ,
       32  'USER_ADDR'         'NAME_TEXTC'     'T_SAIDA' 'USUARIO_REC_FISCAL' 'Usuário Recebimento Fiscal'                 '26' ,
       33  ' '                 ' '              'T_SAIDA' 'REC_FISCAL_FISICO'  'Recebimento Fiscal - Físico (Dias)'         '34' ,
       34  ' '                 ' '              'T_SAIDA' 'HORA_APRO_REQ'      'Hora Aprovação Requisição'                  '10' .



ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.

* Utiliza para retirar os ZEROS.
  IF p_field EQ 'EBELP'.
    wa_estrutura-no_zero = 'X'.
  ENDIF.

  wa_estrutura-outputlen     = p_outputlen.
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

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis.

  PERFORM f_construir_cabecalho USING 'H' text-002.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  CALCULA_DIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EBAN_FRGDT  text
*      -->P_WA_EBAN_BADAT  text
*      <--P_WL_DIAS  text
*----------------------------------------------------------------------*
FORM calcula_dias  USING    p_data1
                            p_data2
                   CHANGING p_dias.

* CALCULO DE DIAS
  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
    EXPORTING
      i_datum_bis             = p_data1
      i_datum_von             = p_data2
    IMPORTING
      e_tage                  = p_dias
    EXCEPTIONS
      days_method_not_defined = 1
      OTHERS                  = 2.

  CONDENSE p_dias NO-GAPS.

*  IF P_DIAS LT 0.
*    MULTIPLY P_DIAS BY -1.
*  ENDIF.

  IF p_dias IS INITIAL.
    p_dias = '0' .
  ENDIF.
*
*  IF P_DATA2 LT P_DATA1.
*    MULTIPLY P_DIAS BY -1.
*  ENDIF.
ENDFORM.                    " CALCULA_DIAS
*&---------------------------------------------------------------------*
*&      Form  Z_EXPORT_SAP_BO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_export_sap_bo .
  DATA vseq TYPE zmmt0099-seq.

  CHECK r_ex_bo IS NOT INITIAL.

  CHECK t_saida IS NOT INITIAL.

  DELETE FROM zmmt0099.
  COMMIT WORK.

  CLEAR vseq.
  vseq = 1.

  LOOP AT t_saida INTO wa_saida.
    MOVE-CORRESPONDING wa_saida TO wa_zmmt0099.

    wa_zmmt0099-seq          =  vseq.
    wa_zmmt0099-usnam_mod    =  sy-uname.
    wa_zmmt0099-data_atual   =  sy-datum.
    wa_zmmt0099-hora_atual   =  sy-uzeit.
    "
    wa_zmmt0099-data_inicio  =  s_badat-low.
    wa_zmmt0099-data_fim     =  s_badat-high.

    APPEND wa_zmmt0099 TO t_zmmt0099.
    CLEAR: wa_saida, wa_zmmt0099.

    vseq =  vseq + 1.
  ENDLOOP.

  INSERT zmmt0099 FROM TABLE t_zmmt0099.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDAR_DEPOSITO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_validar_deposito CHANGING p_erro.

  IF s_lgort[] IS NOT INITIAL.

    SELECT * FROM t001l
      INTO TABLE @DATA(t_t001l)
      WHERE werks IN @s_werks
        AND lgort IN @s_lgort.

    IF t_t001l[] IS INITIAL.
      p_erro = 'X'.
      MESSAGE s000(z_mm) WITH 'Depósito informado não encontrado' DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.
