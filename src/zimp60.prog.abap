*&---------------------------------------------------------------------*
*& Report  ZIMP60
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zimp60.


TABLES: zim01_sol_ap_inv, coas.

DATA: vg_ac_save TYPE c.

DATA:  st01, st02 TYPE c.

DATA: BEGIN OF t_docs_comp,
        aufnr        TYPE kaep_coep_x-aufnr,
        budat        TYPE cobk-budat,
        wogbtr       TYPE kaep_coep_x-wogbtr,
        wkgbtr       TYPE kaep_coep_x-wkgbtr,
        ebeln        TYPE ekpo-ebeln,
        ebelp        TYPE ekpo-ebelp,
        bukrs        TYPE bsis-bukrs,
        belnr_fi     TYPE bsis-belnr,
        gjahr        TYPE bsis-gjahr,
        buzei        TYPE bsis-buzei,
        refbn        TYPE mkpf-mblnr,
        refgj        TYPE mkpf-mjahr,
        awkey        TYPE bkpf-awkey,
        document_key TYPE kaep_document_key_comm,
      END OF t_docs_comp.

DATA: BEGIN OF t_docs_real,
        aufnr        TYPE kaep_coep_x-aufnr,
        budat        TYPE cobk-budat,
        wogbtr       TYPE kaep_coep_x-wogbtr,
        wkgbtr       TYPE kaep_coep_x-wkgbtr,
        ebeln        TYPE ekpo-ebeln,
        ebelp        TYPE ekpo-ebelp,
        bukrs        TYPE bsis-bukrs,
        belnr_fi     TYPE bsis-belnr,
        gjahr        TYPE bsis-gjahr,
        buzei        TYPE bsis-buzei,
        refbn        TYPE mkpf-mblnr,
        refgj        TYPE mkpf-mjahr,
        awkey        TYPE bkpf-awkey,
        document_key LIKE kaep_document_key_actual,
      END OF t_docs_real.

DATA: BEGIN OF t_docs_adto,
        ebeln       TYPE  ebeln,
        ebelp       TYPE  ebelp,
        vgabe       TYPE  vgabe,
        ds_operacao TYPE  char50,
        bukrs       TYPE  bukrs,
        belnr       TYPE  belnr_d,
        gjahr       TYPE  gjahr,
        buzei       TYPE  buzei,
        dmbtr       TYPE  dmbtr,
        dmbe2       TYPE  dmbe2,
        budat       TYPE  budat,
      END OF t_docs_adto.

*-CS2022001163-24.03.2023-#103539-JT-inicio
TYPES: BEGIN OF ty_saida_imob,
         bzdat      TYPE anep-bzdat,
         bwasl      TYPE anep-bwasl,
         bwatxt     TYPE tabwt-bwatxt,
         xtbrl_kob1 TYPE kaep_coac-wkgbtr,
         xtusd_kob1 TYPE kaep_coac-wkgbtr.
TYPES: END  OF ty_saida_imob.

TYPES: BEGIN OF ty_saida_comp,
         bukrs      TYPE anep-bukrs,
         waers      TYPE ekko-waers,
         ebeln      TYPE ekko-ebeln,
         ebelp      TYPE ekpo-ebelp,
         xtbrl_kob2 TYPE kaep_coac-wkgbtr,
         xtusd_kob2 TYPE kaep_coac-wkgbtr.
TYPES: END  OF ty_saida_comp.
*-CS2022001163-24.03.2023-#103539-JT-fim

TYPES: BEGIN OF ty_saida,
         bukrs                 TYPE zim01_sol_ap_inv-bukrs,
         gsber                 TYPE zim01_sol_ap_inv-gsber,
         posnr                 TYPE zim01_sol_ap_inv-posnr,
         kostl                 TYPE zim01_sol_ap_inv-kostl,
         kostl_out             TYPE zim01_sol_ap_inv-kostl,
         ano                   TYPE zim01_sol_ap_inv-ano,
         ltext                 TYPE cskt-ltext,
         descr_item            TYPE zim01_sol_ap_inv-descr_item,
         dt_inicio             TYPE zim01_sol_ap_inv-dt_inicio,
         dt_fim                TYPE zim01_sol_ap_inv-dt_fim,
         objto                 TYPE imaka-anln1,
         objto2                TYPE imaka-anln1,
         ordem                 TYPE imakz-objnr,
         anln2                 TYPE imaka-anln2,
         desc_objto            TYPE coas-ktext,
         aufnr                 TYPE coas-aufnr,
         xivbrl                TYPE zim01_sol_ap_inv-vlr_total,
         xivusd                TYPE zim01_sol_ap_inv-vl_usd,
         xtbrl_kob1            TYPE kaep_coac-wkgbtr,
         xtusd_kob1            TYPE kaep_coac-wkgbtr,
         xtbrl_kob1_ori        TYPE kaep_coac-wkgbtr,
         xtusd_kob1_ori        TYPE kaep_coac-wkgbtr,
         xtbrl_kob2            TYPE kaep_coac-wkgbtr,
         xtusd_kob2            TYPE kaep_coac-wkgbtr,
         xtoad_brl             TYPE bsik-dmbtr,
         xtoad_usd             TYPE bsak-dmbe2,
         disposto_brl          TYPE kaep_coac-wkgbtr,
         disposto_usd          TYPE kaep_coac-wkgbtr,
         disp_brl              TYPE kaep_coac-wkgbtr,
         disp_usd              TYPE kaep_coac-wkgbtr,
         porc_brl              TYPE kaep_coac-wkgbtr,
         porc_usd              TYPE kaep_coac-wkgbtr,
         status                TYPE char2,
         dias_s_mov            TYPE vtbbewe-atage,
*         observacao   TYPE char7,        "US - 62758 - CSB
         texto                 TYPE zim13-oberv,  "US - 62758 - CSB
         anexo                 TYPE char7,
         tabix                 TYPE sy-tabix,
         docs_comp             LIKE t_docs_comp OCCURS 0,
         docs_adto             LIKE t_docs_adto OCCURS 0,
         docs_real             LIKE t_docs_real OCCURS 0,
         objtotipo             TYPE char30,   "*-CS2022001163-24.03.2023-#103539-JT
         color                 TYPE kkblo_specialcol OCCURS 0,
         aibn1                 TYPE anla-aibn1,
         deakt                 TYPE anla-deakt,
         ordstat               TYPE string,
         "          CELLSTYLES   TYPE LVC_T_STYL,
         objnr                 TYPE imakz-objnr,
         classimob             TYPE anla-anlkl,
         solicitacao_invest(6) TYPE c, "US #171475 - MMSILVA - 25.03.2025
       END OF ty_saida.


TYPES: BEGIN OF   ty_zim01_sol_ap_inv,
         bukrs                 TYPE zim01_sol_ap_inv-bukrs,
         gsber                 TYPE zim01_sol_ap_inv-gsber,
         ano                   TYPE zim01_sol_ap_inv-ano,
         safra                 TYPE zim01_sol_ap_inv-safra,
         safra2                TYPE zim01_sol_ap_inv-safra2,
         kostl                 TYPE zim01_sol_ap_inv-kostl,
         buzei                 TYPE zim01_sol_ap_inv-buzei,
         "POSNR      TYPE ZIM01_SOL_AP_INV-POSNR,
         posnr(12)             TYPE c,
         descr_item            TYPE zim01_sol_ap_inv-descr_item,
         vlr_total             TYPE zim01_sol_ap_inv-vlr_total,
         vl_usd                TYPE zim01_sol_ap_inv-vl_usd,
         posnr_out(12)         TYPE c,
         solicitacao_invest(6) TYPE c, "US #171475 - MMSILVA - 25.03.2025
         anln1                 TYPE anla-anln1, "US #171475 - MMSILVA - 25.03.2025
       END OF  ty_zim01_sol_ap_inv.

TYPES: BEGIN OF ty_cskt,
         spras TYPE  cskt-spras,
         kokrs TYPE  cskt-kokrs,
         kostl TYPE  cskt-kostl,
         datbi TYPE  cskt-datbi,
         ltext TYPE  cskt-ltext,
       END OF ty_cskt.

TYPES: BEGIN OF ty_coas,
         aufnr TYPE coas-aufnr,
         ktext TYPE coas-ktext,
* ---->  US #171475 - MMSILVA - 10.04.2025 - Inicio
         user2 TYPE coas-user2,
         bukrs TYPE coas-bukrs,
* ---->  US #171475 - MMSILVA - 10.04.2025 - Fim
       END OF ty_coas.

TYPES: BEGIN OF ty_anla,
         bukrs TYPE anla-bukrs,
         anln1 TYPE anla-anln1,
         anln2 TYPE anla-anln2,
         txt50 TYPE anla-txt50,
         lkauf TYPE anla-lkauf,
         aibn1 TYPE anla-aibn1,
         deakt TYPE anla-deakt,
       END OF ty_anla.

TYPES: BEGIN OF ty_anep,
         bukrs TYPE  anep-bukrs,
         anln1 TYPE  anep-anln1,
         anln2 TYPE  anep-anln2,
         lnran TYPE  anep-lnran,
         afabe TYPE  anep-afabe,
         bwasl TYPE  anep-bwasl,
         bzdat TYPE  anep-bzdat,
         anbtr TYPE  anep-anbtr,
       END OF ty_anep.


TYPES: BEGIN OF ty_ekkn,
         aufnr TYPE   ekkn-aufnr,
         anln1 TYPE   ekkn-anln1,
         anln2 TYPE   ekkn-anln2,
         ebeln TYPE   ekkn-ebeln,
         ebelp TYPE   ekkn-ebelp,
       END OF ty_ekkn.

TYPES: BEGIN OF ty_ekbe,
         ebeln    TYPE  ekbe-ebeln,
         ebelp    TYPE  ekbe-ebelp,
         vgabe    TYPE  ekbe-vgabe,
         belnr    TYPE  ekbe-belnr,
         buzei    TYPE  ekbe-buzei,
         gjahr    TYPE  ekbe-gjahr,
         buzei_fi TYPE  bsik-buzei,
         budat    TYPE  ekbe-budat,
       END OF ty_ekbe.

TYPES: BEGIN OF ty_bsik,
         bukrs TYPE  bsik-bukrs,
         belnr TYPE  bsik-belnr,
         buzei TYPE  bsik-buzei,
         gjahr TYPE  bsik-gjahr,
         budat TYPE  bsik-budat,
         shkzg TYPE  bsik-shkzg,
         dmbtr TYPE  bsik-dmbtr,
         dmbe2 TYPE  bsik-dmbe2,
       END OF ty_bsik.


TYPES: BEGIN OF ty_bsak,
         bukrs TYPE   bsak-bukrs,
         belnr TYPE   bsak-belnr,
         buzei TYPE   bsak-buzei,
         gjahr TYPE   bsak-gjahr,
         budat TYPE   bsak-budat,
         shkzg TYPE   bsak-shkzg,
         dmbtr TYPE   bsak-dmbtr,
         dmbe2 TYPE   bsak-dmbe2,
       END OF ty_bsak.

DATA: BEGIN OF tg_entrada OCCURS 0,
        kokrs          TYPE cobk-kokrs,
        belnr          TYPE cobk-belnr,
        gjahr          TYPE cobk-gjahr,
        versn          TYPE cobk-versn,
        vrgng          TYPE cobk-vrgng,
        timestmp       TYPE cobk-timestmp,
        perab          TYPE cobk-perab,
        perbi          TYPE cobk-perbi,
        bldat          TYPE cobk-bldat,
        budat          TYPE cobk-budat,
        cpudt          TYPE cobk-cpudt,
        usnam          TYPE cobk-usnam,
        bltxt          TYPE cobk-bltxt,
        stflg          TYPE cobk-stflg,
        stokz          TYPE cobk-stokz,
        refbt          TYPE cobk-refbt,
        refbn          TYPE cobk-refbn,
        refbk          TYPE cobk-refbk,
        refgj          TYPE cobk-refgj,
        blart          TYPE cobk-blart,
        orgvg          TYPE cobk-orgvg,
        sumbz          TYPE cobk-sumbz,
        delbz          TYPE cobk-delbz,
        wsdat          TYPE cobk-wsdat,
        kurst          TYPE cobk-kurst,
        varnr          TYPE cobk-varnr,
        kwaer          TYPE cobk-kwaer,
        ctyp1          TYPE cobk-ctyp1,
        ctyp2          TYPE cobk-ctyp2,
        ctyp3          TYPE cobk-ctyp3,
        ctyp4          TYPE cobk-ctyp4,
        awtyp          TYPE cobk-awtyp,
        aworg          TYPE cobk-aworg,
        logsystem      TYPE cobk-logsystem,
        cputm          TYPE cobk-cputm,
        alebz          TYPE cobk-alebz,
        alebn          TYPE cobk-alebn,
        awsys          TYPE cobk-awsys,
        awref_rev      TYPE cobk-awref_rev,
        aworg_rev      TYPE cobk-aworg_rev,
        valdt          TYPE cobk-valdt,
        wrttp          TYPE kaep_coep_x-wrttp,
        beknz          TYPE kaep_coep_x-beknz,
        hrkft          TYPE kaep_coep_x-hrkft,
        buzei          TYPE kaep_coep_x-buzei,
        zlenr          TYPE kaep_coep_x-zlenr,
        bw_refbz       TYPE kaep_coep_x-bw_refbz,
        perio          TYPE kaep_coep_x-perio,
        mvflg          TYPE kaep_coep_x-mvflg,
        muvflg         TYPE kaep_coep_x-muvflg,
        bwaer          TYPE kaep_coep_x-bwaer,
        sgtxt          TYPE kaep_coep_x-sgtxt,
        refbz          TYPE kaep_coep_x-refbz,
        ebeln          TYPE kaep_coep_x-ebeln,
        ebelp          TYPE kaep_coep_x-ebelp,
        ebtxt          TYPE kaep_coep_x-ebtxt,
        zekkn          TYPE kaep_coep_x-zekkn,
        btrkl          TYPE kaep_coep_x-btrkl,
        beltp          TYPE kaep_coep_x-beltp,
        refbz_fi       TYPE kaep_coep_x-refbz_fi,
        qmnum          TYPE kaep_coep_x-qmnum,
        qmtxt          TYPE kaep_coep_x-qmtxt,
        lednr          TYPE kaep_coep_x-lednr,
        gsber          TYPE kaep_coep_x-gsber,
        vbund          TYPE kaep_coep_x-vbund,
        pargb          TYPE kaep_coep_x-pargb,
        bukrs          TYPE kaep_coep_x-bukrs,
        scope          TYPE kaep_coep_x-scope,
        pbukrs         TYPE kaep_coep_x-pbukrs,
        pscope         TYPE kaep_coep_x-pscope,
        objnr          TYPE kaep_coep_x-objnr,
        objid          TYPE kaep_coep_x-objid,
        obj_txt        TYPE kaep_coep_x-obj_txt,
        obart          TYPE kaep_coep_x-obart,
        objart_txt     TYPE kaep_coep_x-objart_txt,
        kostl          TYPE kaep_coep_x-kostl,
        aufnr          TYPE kaep_coep_x-aufnr,
        vornr_auf      TYPE kaep_coep_x-vornr_auf,
        posid          TYPE kaep_coep_x-posid,
        pspid          TYPE kaep_coep_x-pspid,
        lstar          TYPE kaep_coep_x-lstar,
        prznr          TYPE kaep_coep_x-prznr,
        kstrg          TYPE kaep_coep_x-kstrg,
        vbeln          TYPE kaep_coep_x-vbeln,
        posnr          TYPE kaep_coep_x-posnr,
        matricula      TYPE kaep_coep_x-matricula,
        ordemcarreg    TYPE kaep_coep_x-ordemcarreg,
        placa          TYPE kaep_coep_x-placa,
        pobid_umb      TYPE kaep_coep_x-pobid_umb,
        pob_umb_txt    TYPE kaep_coep_x-pob_umb_txt,
        pobart_umb     TYPE kaep_coep_x-pobart_umb,
        pobart_txt_umb TYPE kaep_coep_x-pobart_txt_umb,
        parob1         TYPE kaep_coep_x-parob1,
        pobid          TYPE kaep_coep_x-pobid,
        pob_txt        TYPE kaep_coep_x-pob_txt,
        pobart         TYPE kaep_coep_x-pobart,
        pobart_txt     TYPE kaep_coep_x-pobart_txt,
        prtko          TYPE kaep_coep_x-prtko,
        prtau          TYPE kaep_coep_x-prtau,
        prtps          TYPE kaep_coep_x-prtps,
        prtpo          TYPE kaep_coep_x-prtpo,
        prtls          TYPE kaep_coep_x-prtls,
        prtpr          TYPE kaep_coep_x-prtpr,
        prtkt          TYPE kaep_coep_x-prtkt,
        prtvr          TYPE kaep_coep_x-prtvr,
        uspob          TYPE kaep_coep_x-uspob,
        uobid          TYPE kaep_coep_x-uobid,
        uob_txt        TYPE kaep_coep_x-uob_txt,
        uobart         TYPE kaep_coep_x-uobart,
        uobart_txt     TYPE kaep_coep_x-uobart_txt,
        objnr_n1       TYPE kaep_coep_x-objnr_n1,
        objnr_n2       TYPE kaep_coep_x-objnr_n2,
        objnr_n3       TYPE kaep_coep_x-objnr_n3,
        paobjnr        TYPE kaep_coep_x-paobjnr,
        kstar          TYPE kaep_coep_x-kstar,
        cel_ktxt       TYPE kaep_coep_x-cel_ktxt,
        cel_ltxt       TYPE kaep_coep_x-cel_ltxt,
        gkont          TYPE kaep_coep_x-gkont,
        gkoar          TYPE kaep_coep_x-gkoar,
        gkont_ktxt     TYPE kaep_coep_x-gkont_ktxt,
        gkont_ltxt     TYPE kaep_coep_x-gkont_ltxt,
        mat_txt        TYPE kaep_coep_x-mat_txt,
        twaer          TYPE kaep_coep_x-twaer,
        owaer          TYPE kaep_coep_x-owaer,
        meinh          TYPE kaep_coep_x-meinh,
        meinb          TYPE kaep_coep_x-meinb,
        wtgbtr         TYPE kaep_coep_x-wtgbtr,
        wogbtr         TYPE kaep_coep_x-wogbtr,
        wkgbtr         TYPE kaep_coep_x-wkgbtr,
        wkfbtr         TYPE kaep_coep_x-wkfbtr,
        rwaer          TYPE kaep_coep_x-rwaer,
        wrgbtr         TYPE kaep_coep_x-wrgbtr,
        wtfbtr         TYPE kaep_coep_x-wtfbtr,
        wofbtr         TYPE kaep_coep_x-wofbtr,
        wrfbtr         TYPE kaep_coep_x-wrfbtr,
        wkvbtr         TYPE kaep_coep_x-wkvbtr,
        wtvbtr         TYPE kaep_coep_x-wtvbtr,
        wovbtr         TYPE kaep_coep_x-wovbtr,
        wrvbtr         TYPE kaep_coep_x-wrvbtr,
        pagbtr         TYPE kaep_coep_x-pagbtr,
        pafbtr         TYPE kaep_coep_x-pafbtr,
        megbtr         TYPE kaep_coep_x-megbtr,
        mefbtr         TYPE kaep_coep_x-mefbtr,
        mbgbtr         TYPE kaep_coep_x-mbgbtr,
        mbfbtr         TYPE kaep_coep_x-mbfbtr,
        werks          TYPE kaep_coep_x-werks,
        matnr          TYPE kaep_coep_x-matnr,
        hkgrp          TYPE kaep_coep_x-hkgrp,
        pcver          TYPE kaep_coep_x-pcver,
        cbrke          TYPE kaep_coep_x-cbrke,
        cbrki          TYPE kaep_coep_x-cbrki,
        cbart          TYPE kaep_coep_x-cbart,
        drvtp          TYPE kaep_coep_x-drvtp,
        cbat           TYPE kaep_coep_x-cbat,
        recind         TYPE kaep_coep_x-recind,
        bemot          TYPE kaep_coep_x-bemot,
        pernr          TYPE kaep_coep_x-pernr,
        rsrce          TYPE kaep_coep_x-rsrce,
        fkber          TYPE kaep_coep_x-fkber,
        pfkber         TYPE kaep_coep_x-pfkber,
        geber          TYPE kaep_coep_x-geber,
        pgeber         TYPE kaep_coep_x-pgeber,
        grant_nbr      TYPE kaep_coep_x-grant_nbr,
        pgrant_nbr     TYPE kaep_coep_x-pgrant_nbr,
        segment        TYPE kaep_coep_x-segment,
        psegment       TYPE kaep_coep_x-psegment,
        budget_pd      TYPE kaep_coep_x-budget_pd,
        pbudget_pd     TYPE kaep_coep_x-pbudget_pd,
        prodper        TYPE kaep_coep_x-prodper,
        objgrp         TYPE kaep_coep_x-objgrp,
        objgrp_txt     TYPE kaep_coep_x-objgrp_txt,
        objgrp_ind     TYPE kaep_coep_x-objgrp_ind,
        celgrp         TYPE kaep_coep_x-celgrp,
        celgrp_txt     TYPE kaep_coep_x-celgrp_txt,
        celgrp_ind     TYPE kaep_coep_x-celgrp_ind,
        segname        TYPE kaep_coep_x-segname,
        selkz          TYPE kaep_coep_x-selkz,
        wtgres         TYPE kaep_coep_x-wtgres,
        wogres         TYPE kaep_coep_x-wogres,
        wkgres         TYPE kaep_coep_x-wkgres,
        wkfres         TYPE kaep_coep_x-wkfres,
        bureg          TYPE kaep_coep_x-bureg,
        abwst          TYPE kaep_coep_x-abwst,
        afabe          TYPE kaep_coep_x-afabe,
        anbres         TYPE kaep_coep_x-anbres,
        awaers         TYPE kaep_coep_x-awaers,
        vbureg         TYPE kaep_coep_x-vbureg,
        abureg         TYPE kaep_coep_x-abureg,
        abwnr          TYPE kaep_coep_x-abwnr,
        abgkz          TYPE kaep_coep_x-abgkz,
        longnum        TYPE kaep_coep_x-longnum,
*---> 29/05/2023 - Migração S4 - JS
        "nondb          TYPE kaep_coep_x-nondb,
        nondb          TYPE char1,
*<--- 26/05/2023 - Migração S4 - JS
        dabrz          TYPE kaep_coep_x-dabrz,
        workf(1)       TYPE c,
        flgvor         LIKE coepx-flgvor,
        flgabg         LIKE coepx-flgabg,
        flgup1(1)      TYPE c,
        flgup2(1)      TYPE c,
        flgup3(1)      TYPE c,
        abwnr1         LIKE coepx-abwnr1,
        abwnr2         LIKE coepx-abwnr2,
        abwnr3         LIKE coepx-abwnr3,
        ampel(1)       TYPE c.
DATA: END OF tg_entrada.

DATA: BEGIN OF tg_entrada_02 OCCURS 0,
        kokrs          TYPE cobk-kokrs,
        belnr          TYPE cobk-belnr,
        gjahr          TYPE cobk-gjahr,
        versn          TYPE cobk-versn,
        vrgng          TYPE cobk-vrgng,
        timestmp       TYPE cobk-timestmp,
        perab          TYPE cobk-perab,
        perbi          TYPE cobk-perbi,
        bldat          TYPE cobk-bldat,
        budat          TYPE cobk-budat,
        cpudt          TYPE cobk-cpudt,
        usnam          TYPE cobk-usnam,
        bltxt          TYPE cobk-bltxt,
        stflg          TYPE cobk-stflg,
        stokz          TYPE cobk-stokz,
        refbt          TYPE co_refbtyp,
        refbn          TYPE cobk-refbn,
        rfpos          TYPE cc_rfpos,
        refbk          TYPE cobk-refbk,
        refgj          TYPE cobk-refgj,
        blart          TYPE cobk-blart,
        orgvg          TYPE cobk-orgvg,
        sumbz          TYPE cobk-sumbz,
        delbz          TYPE cobk-delbz,
        wsdat          TYPE cobk-wsdat,
        kurst          TYPE cobk-kurst,
        varnr          TYPE cobk-varnr,
        kwaer          TYPE cobk-kwaer,
        ctyp1          TYPE cobk-ctyp1,
        ctyp2          TYPE cobk-ctyp2,
        ctyp3          TYPE cobk-ctyp3,
        ctyp4          TYPE cobk-ctyp4,
        awtyp          TYPE cobk-awtyp,
        aworg          TYPE cobk-aworg,
        logsystem      TYPE cobk-logsystem,
        cputm          TYPE cobk-cputm,
        alebz          TYPE cobk-alebz,
        alebn          TYPE cobk-alebn,
        awsys          TYPE cobk-awsys,
        awref_rev      TYPE cobk-awref_rev,
        aworg_rev      TYPE cobk-aworg_rev,
        valdt          TYPE cobk-valdt,
        wrttp          TYPE kaep_coep_x-wrttp,
        beknz          TYPE kaep_coep_x-beknz,
        hrkft          TYPE kaep_coep_x-hrkft,
        buzei          TYPE kaep_coep_x-buzei,
        zlenr          TYPE kaep_coep_x-zlenr,
        bw_refbz       TYPE kaep_coep_x-bw_refbz,
        perio          TYPE kaep_coep_x-perio,
        mvflg          TYPE kaep_coep_x-mvflg,
        muvflg         TYPE kaep_coep_x-muvflg,
        bwaer          TYPE kaep_coep_x-bwaer,
        sgtxt          TYPE kaep_coep_x-sgtxt,
        refbz          TYPE kaep_coep_x-refbz,
        ebeln          TYPE kaep_coep_x-ebeln,
        ebelp          TYPE kaep_coep_x-ebelp,
        ebtxt          TYPE kaep_coep_x-ebtxt,
        zekkn          TYPE kaep_coep_x-zekkn,
        btrkl          TYPE kaep_coep_x-btrkl,
        beltp          TYPE kaep_coep_x-beltp,
        refbz_fi       TYPE kaep_coep_x-refbz_fi,
        qmnum          TYPE kaep_coep_x-qmnum,
        qmtxt          TYPE kaep_coep_x-qmtxt,
        lednr          TYPE kaep_coep_x-lednr,
        gsber          TYPE kaep_coep_x-gsber,
        vbund          TYPE kaep_coep_x-vbund,
        pargb          TYPE kaep_coep_x-pargb,
        bukrs          TYPE kaep_coep_x-bukrs,
        scope          TYPE kaep_coep_x-scope,
        pbukrs         TYPE kaep_coep_x-pbukrs,
        pscope         TYPE kaep_coep_x-pscope,
        objnr          TYPE kaep_coep_x-objnr,
        objid          TYPE kaep_coep_x-objid,
        obj_txt        TYPE kaep_coep_x-obj_txt,
        obart          TYPE kaep_coep_x-obart,
        objart_txt     TYPE kaep_coep_x-objart_txt,
        kostl          TYPE kaep_coep_x-kostl,
        aufnr          TYPE kaep_coep_x-aufnr,
        vornr_auf      TYPE kaep_coep_x-vornr_auf,
        posid          TYPE kaep_coep_x-posid,
        pspid          TYPE kaep_coep_x-pspid,
        lstar          TYPE kaep_coep_x-lstar,
        prznr          TYPE kaep_coep_x-prznr,
        kstrg          TYPE kaep_coep_x-kstrg,
        vbeln          TYPE kaep_coep_x-vbeln,
        posnr          TYPE kaep_coep_x-posnr,
        matricula      TYPE kaep_coep_x-matricula,
        ordemcarreg    TYPE kaep_coep_x-ordemcarreg,
        placa          TYPE kaep_coep_x-placa,
        pobid_umb      TYPE kaep_coep_x-pobid_umb,
        pob_umb_txt    TYPE kaep_coep_x-pob_umb_txt,
        pobart_umb     TYPE kaep_coep_x-pobart_umb,
        pobart_txt_umb TYPE kaep_coep_x-pobart_txt_umb,
        parob1         TYPE kaep_coep_x-parob1,
        pobid          TYPE kaep_coep_x-pobid,
        pob_txt        TYPE kaep_coep_x-pob_txt,
        pobart         TYPE kaep_coep_x-pobart,
        pobart_txt     TYPE kaep_coep_x-pobart_txt,
        prtko          TYPE kaep_coep_x-prtko,
        prtau          TYPE kaep_coep_x-prtau,
        prtps          TYPE kaep_coep_x-prtps,
        prtpo          TYPE kaep_coep_x-prtpo,
        prtls          TYPE kaep_coep_x-prtls,
        prtpr          TYPE kaep_coep_x-prtpr,
        prtkt          TYPE kaep_coep_x-prtkt,
        prtvr          TYPE kaep_coep_x-prtvr,
        uspob          TYPE kaep_coep_x-uspob,
        uobid          TYPE kaep_coep_x-uobid,
        uob_txt        TYPE kaep_coep_x-uob_txt,
        uobart         TYPE kaep_coep_x-uobart,
        uobart_txt     TYPE kaep_coep_x-uobart_txt,
        objnr_n1       TYPE kaep_coep_x-objnr_n1,
        objnr_n2       TYPE kaep_coep_x-objnr_n2,
        objnr_n3       TYPE kaep_coep_x-objnr_n3,
        paobjnr        TYPE kaep_coep_x-paobjnr,
        kstar          TYPE kaep_coep_x-kstar,
        cel_ktxt       TYPE kaep_coep_x-cel_ktxt,
        cel_ltxt       TYPE kaep_coep_x-cel_ltxt,
        gkont          TYPE kaep_coep_x-gkont,
        gkoar          TYPE kaep_coep_x-gkoar,
        gkont_ktxt     TYPE kaep_coep_x-gkont_ktxt,
        gkont_ltxt     TYPE kaep_coep_x-gkont_ltxt,
        mat_txt        TYPE kaep_coep_x-mat_txt,
        twaer          TYPE kaep_coep_x-twaer,
        owaer          TYPE kaep_coep_x-owaer,
        meinh          TYPE kaep_coep_x-meinh,
        meinb          TYPE kaep_coep_x-meinb,
        wtgbtr         TYPE kaep_coep_x-wtgbtr,
        wogbtr         TYPE kaep_coep_x-wogbtr,
        wkgbtr         TYPE kaep_coep_x-wkgbtr,
        wkfbtr         TYPE kaep_coep_x-wkfbtr,
        rwaer          TYPE kaep_coep_x-rwaer,
        wrgbtr         TYPE kaep_coep_x-wrgbtr,
        wtfbtr         TYPE kaep_coep_x-wtfbtr,
        wofbtr         TYPE kaep_coep_x-wofbtr,
        wrfbtr         TYPE kaep_coep_x-wrfbtr,
        wkvbtr         TYPE kaep_coep_x-wkvbtr,
        wtvbtr         TYPE kaep_coep_x-wtvbtr,
        wovbtr         TYPE kaep_coep_x-wovbtr,
        wrvbtr         TYPE kaep_coep_x-wrvbtr,
        pagbtr         TYPE kaep_coep_x-pagbtr,
        pafbtr         TYPE kaep_coep_x-pafbtr,
        megbtr         TYPE kaep_coep_x-megbtr,
        mefbtr         TYPE kaep_coep_x-mefbtr,
        mbgbtr         TYPE kaep_coep_x-mbgbtr,
        mbfbtr         TYPE kaep_coep_x-mbfbtr,
        werks          TYPE kaep_coep_x-werks,
        matnr          TYPE kaep_coep_x-matnr,
        hkgrp          TYPE kaep_coep_x-hkgrp,
        pcver          TYPE kaep_coep_x-pcver,
        cbrke          TYPE kaep_coep_x-cbrke,
        cbrki          TYPE kaep_coep_x-cbrki,
        cbart          TYPE kaep_coep_x-cbart,
        drvtp          TYPE kaep_coep_x-drvtp,
        cbat           TYPE kaep_coep_x-cbat,
        recind         TYPE kaep_coep_x-recind,
        bemot          TYPE kaep_coep_x-bemot,
        pernr          TYPE kaep_coep_x-pernr,
        rsrce          TYPE kaep_coep_x-rsrce,
        fkber          TYPE kaep_coep_x-fkber,
        pfkber         TYPE kaep_coep_x-pfkber,
        geber          TYPE kaep_coep_x-geber,
        pgeber         TYPE kaep_coep_x-pgeber,
        grant_nbr      TYPE kaep_coep_x-grant_nbr,
        pgrant_nbr     TYPE kaep_coep_x-pgrant_nbr,
        segment        TYPE kaep_coep_x-segment,
        psegment       TYPE kaep_coep_x-psegment,
        budget_pd      TYPE kaep_coep_x-budget_pd,
        pbudget_pd     TYPE kaep_coep_x-pbudget_pd,
        prodper        TYPE kaep_coep_x-prodper,
        objgrp         TYPE kaep_coep_x-objgrp,
        objgrp_txt     TYPE kaep_coep_x-objgrp_txt,
        objgrp_ind     TYPE kaep_coep_x-objgrp_ind,
        celgrp         TYPE kaep_coep_x-celgrp,
        celgrp_txt     TYPE kaep_coep_x-celgrp_txt,
        celgrp_ind     TYPE kaep_coep_x-celgrp_ind,
        segname        TYPE kaep_coep_x-segname,
        selkz          TYPE kaep_coep_x-selkz,
        wtgres         TYPE kaep_coep_x-wtgres,
        wogres         TYPE kaep_coep_x-wogres,
        wkgres         TYPE kaep_coep_x-wkgres,
        wkfres         TYPE kaep_coep_x-wkfres,
        bureg          TYPE kaep_coep_x-bureg,
        abwst          TYPE kaep_coep_x-abwst,
        afabe          TYPE kaep_coep_x-afabe,
        anbres         TYPE kaep_coep_x-anbres,
        awaers         TYPE kaep_coep_x-awaers,
        vbureg         TYPE kaep_coep_x-vbureg,
        abureg         TYPE kaep_coep_x-abureg,
        abwnr          TYPE kaep_coep_x-abwnr,
        abgkz          TYPE kaep_coep_x-abgkz,
        longnum        TYPE kaep_coep_x-longnum,
*---> 29/05/2023 - Migração S4 - JS
        "nondb          TYPE kaep_coep_x-nondb,
        nondb          TYPE char1,
*<--- 29/05/2023 - Migração S4 - JS
        dabrz          TYPE kaep_coep_x-dabrz,
        workf(1)       TYPE c,
        flgvor         LIKE coepx-flgvor,
        flgabg         LIKE coepx-flgabg,
        flgup1(1)      TYPE c,
        flgup2(1)      TYPE c,
        flgup3(1)      TYPE c,
        abwnr1         LIKE coepx-abwnr1,
        abwnr2         LIKE coepx-abwnr2,
        abwnr3         LIKE coepx-abwnr3,
        ampel(1)       TYPE c.
DATA: END OF tg_entrada_02.

TYPES: BEGIN OF ty_kob1,
         aufnr        TYPE kaep_coep_x-aufnr,
         budat        TYPE cobk-budat,
         bldat        TYPE cobk-bldat,
         wogbtr       TYPE kaep_coep_x-wogbtr,
         wkgbtr       TYPE kaep_coep_x-wkgbtr,
         ebeln        TYPE ekpo-ebeln,
         ebelp        TYPE ekpo-ebelp,
         bukrs        TYPE bsis-bukrs,
         belnr_fi     TYPE bsis-belnr,
         gjahr        TYPE bsis-gjahr,
         buzei        TYPE bsis-buzei,
         refbn        TYPE mkpf-mblnr,
         refgj        TYPE mkpf-mjahr,
         awkey        TYPE bkpf-awkey,
         document_key LIKE kaep_document_key_actual,
       END OF ty_kob1.


TYPES: BEGIN OF ty_kob2,
         aufnr        TYPE kaep_coep_x-aufnr,
         budat        TYPE cobk-budat,
         bldat        TYPE cobk-bldat,
         wogbtr       TYPE kaep_coep_x-wogbtr,
         wkgbtr       TYPE kaep_coep_x-wkgbtr,
         ebeln        TYPE ekpo-ebeln,
         ebelp        TYPE ekpo-ebelp,
         bukrs        TYPE bsis-bukrs,
         belnr_fi     TYPE bsis-belnr,
         gjahr        TYPE bsis-gjahr,
         buzei        TYPE bsis-buzei,
         refbn        TYPE mkpf-mblnr,
         refgj        TYPE mkpf-mjahr,
         awkey        TYPE bkpf-awkey,
         document_key TYPE kaep_document_key_comm,
       END OF ty_kob2.

TYPES: BEGIN OF ty_ordem,
         posnr                 TYPE imakz-posnr,
         objnr                 TYPE imakz-objnr,
         lfdnr                 TYPE imaka-lfdnr,
         bukrs                 TYPE imaka-bukrs,
         anln2                 TYPE imaka-anln2,
         equnr                 TYPE imaka-equnr,
         solicitacao_invest(6) TYPE c, "US #171475 - MMSILVA - 25.03.2025
       END OF ty_ordem.

TYPES: BEGIN OF ty_bkpf_aux,
         belnr    TYPE ekbe-belnr,
         gjahr    TYPE ekbe-gjahr,
         awkey    TYPE bkpf-awkey,
         buzei_fi TYPE ekbe-buzei,
         budat    TYPE  ekbe-budat,
       END OF ty_bkpf_aux.

TYPES: BEGIN OF ty_ekpo_imob,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         waers TYPE ekko-waers,
         netpr TYPE ekpo-netpr.
TYPES: END   OF ty_ekpo_imob.

TYPES: BEGIN OF ty_ekbe_imob,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         zekkn TYPE ekbe-zekkn,
         vgabe TYPE ekbe-vgabe,
         gjahr TYPE ekbe-gjahr,
         belnr TYPE ekbe-belnr,
         buzei TYPE ekbe-buzei.
TYPES: END   OF ty_ekbe_imob.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION


DATA: it_saida             TYPE TABLE OF ty_saida,
      it_saida_aux         TYPE TABLE OF ty_saida,
      it_saida_ativo       TYPE TABLE OF ty_saida,
      it_saida_imob        TYPE TABLE OF ty_saida_imob,
      it_saida_comp        TYPE TABLE OF ty_saida_comp,
      it_zim01_sol_ap_inv  TYPE TABLE OF ty_zim01_sol_ap_inv,
      it_zim01_aux         TYPE TABLE OF ty_zim01_sol_ap_inv,
      it_zim01             TYPE TABLE OF ty_zim01_sol_ap_inv WITH HEADER LINE,
      it_zim01_ano_min     TYPE TABLE OF ty_zim01_sol_ap_inv WITH HEADER LINE,
      it_tka02             TYPE TABLE OF tka02,
      it_cskt              TYPE TABLE OF cskt,
      it_imakz             TYPE TABLE OF imakz,
      it_coas              TYPE TABLE OF ty_coas,
      it_imaka             TYPE TABLE OF imaka,
      it_tabwt             TYPE TABLE OF tabwt,
      wa_tabwt             TYPE tabwt,
      it_anla              TYPE TABLE OF ty_anla,
      it_anep              TYPE TABLE OF ty_anep,
      it_anep_usd          TYPE TABLE OF ty_anep,
      it_anep_imob         TYPE TABLE OF ty_anep,  "*-CS2022001163-24.03.2023-#103539-JT
      it_anep_usd_imob     TYPE TABLE OF ty_anep,  "*-CS2022001163-24.03.2023-#103539-JT
      it_anep_imob_ger     TYPE TABLE OF ty_anep,  "*-CS2022001163-24.03.2023-#103539-JT
      it_anep_usd_imob_ger TYPE TABLE OF ty_anep,  "*-CS2022001163-24.03.2023-#103539-JT
      it_ekkn_imob         TYPE TABLE OF ekkn,
      it_ekkn_comp         TYPE TABLE OF ekkn,
      it_ekpo_imob         TYPE TABLE OF ty_ekpo_imob,
      it_ekbe_imob         TYPE TABLE OF ty_ekbe_imob,
      it_ekpo_comp         TYPE TABLE OF ty_ekpo_imob,
      it_ekbe_comp         TYPE TABLE OF ty_ekbe_imob,
      it_eket_imob         TYPE TABLE OF eket,
      wa_eket_imob         TYPE eket,
      it_eket_comp         TYPE TABLE OF eket,
      wa_eket_comp         TYPE eket,
      wa_ekkn_imob         TYPE ekkn,
      wa_ekkn_comp         TYPE ekkn,
      wa_ekpo_imob         TYPE ty_ekpo_imob,
      wa_ekbe_imob         TYPE ty_ekbe_imob,
      wa_ekpo_comp         TYPE ty_ekpo_imob,
      wa_ekbe_comp         TYPE ty_ekbe_imob,
      it_anla_imob         TYPE TABLE OF ty_anla,
      wa_anla_imob         TYPE ty_anla,
*
      it_ekkn              TYPE TABLE OF ty_ekkn,
      it_bkpf_aux          TYPE TABLE OF ty_bkpf_aux,
      it_ekbe              TYPE TABLE OF ty_ekbe,
      it_bsik              TYPE TABLE OF ty_bsik,
      it_bsak              TYPE TABLE OF ty_bsak,
      it_zim13             TYPE TABLE OF zim13,
      it_kob1              TYPE TABLE OF ty_kob1,
      it_kob2              TYPE TABLE OF ty_kob2,
      t_ordem              TYPE TABLE OF ty_ordem,
      it_bkpf              TYPE TABLE OF bkpf,
      tg_bkpf_tmp          TYPE TABLE OF bkpf WITH HEADER LINE,
      wa_saida             TYPE ty_saida,
      wa_saida_ativo       TYPE ty_saida,
      wa_saida_imob        TYPE ty_saida_imob,
      wa_saida_comp        TYPE ty_saida_comp,
      wa_zim01_sol_ap_inv  TYPE ty_zim01_sol_ap_inv,
      wa_zim01             TYPE ty_zim01_sol_ap_inv,
      wa_tka02             TYPE tka02,
      wa_cskt              TYPE cskt,
      wa_imakz             TYPE imakz,
      wa_coas              TYPE ty_coas,
      wa_imaka             TYPE imaka,
      wa_anla              TYPE ty_anla,
      wa_anep              TYPE ty_anep,
      wa_anep_usd          TYPE ty_anep,
      wa_anep_imob         TYPE ty_anep,
      wa_anep_usd_imob     TYPE ty_anep,
      wa_ekkn              TYPE ty_ekkn,
      wa_ekbe              TYPE ty_ekbe,
      wa_bsik              TYPE ty_bsik,
      wa_bsak              TYPE ty_bsak,
      wa_zim13             TYPE zim13,
      wa_kob1              TYPE ty_kob1,
      wa_kob2              TYPE ty_kob2,
      wa_ordem             TYPE ty_ordem.

DATA: it_texto TYPE STANDARD TABLE OF tline,
      wa_texto TYPE tline,
      tl_texto TYPE catsxt_longtext_itab,
      wl_texto TYPE LINE OF catsxt_longtext_itab,
      wl_name  TYPE thead-tdname.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_tabix           TYPE sy-tabix,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
      dg_parent_1        TYPE REF TO cl_gui_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_2        TYPE REF TO cl_gui_container,
      dg_parent_2a       TYPE REF TO cl_gui_container,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      obj_toolbar        TYPE REF TO lcl_alv_toolbar,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      variante           LIKE disvariant,
      gs_variant_c       TYPE disvariant,
      picture            TYPE REF TO cl_gui_picture,
      it_exclude_fcode   TYPE ui_functions,
      wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      ctl_alv            TYPE REF TO cl_gui_alv_grid,
      table_element      TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      table_element2     TYPE REF TO cl_dd_table_element,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area,
      dg_html_cntrl      TYPE REF TO cl_gui_html_viewer,
      ls_stable          TYPE lvc_s_stbl.

DATA: wa_stable       TYPE lvc_s_stbl VALUE 'XX',
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.

DATA: wa_estrutura     TYPE ty_estrutura,
      estrutura        TYPE TABLE OF ty_estrutura,
      it_fieldcat_imob TYPE TABLE OF ty_estrutura,
      wa_fieldcat_imob TYPE ty_estrutura,
      it_fieldcat_comp TYPE TABLE OF ty_estrutura,
      wa_fieldcat_comp TYPE ty_estrutura.

DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: it_ordem     TYPE RANGE OF coas-aufnr,
      w_ordem      LIKE LINE OF it_ordem,
      it_objnr     TYPE RANGE OF imaka-anln1,
      w_objnr      LIKE LINE OF it_objnr,
      it_vposnr    TYPE RANGE OF char12,
      wa_vposnr    LIKE LINE OF it_vposnr,
      it_aufnr     TYPE RANGE OF imakz-objnr,
      wa_aufnr     LIKE LINE OF it_aufnr,
      xtbrl_kob1   TYPE kaep_coac-wkgbtr,
      xtusd_kob1   TYPE kaep_coac-wkgbtr,
      xtbrl_kob2   TYPE kaep_coac-wkgbtr,
      xtusd_kob2   TYPE kaep_coac-wkgbtr,
      xtotbrl_imob TYPE anep-anbtr,
      xtotusd_imob TYPE anep-anbtr,
      xtoad_brl    TYPE bsik-dmbtr,
      xtoad_usd    TYPE bsik-dmbtr,
      xivbrl       TYPE zim01_sol_ap_inv-vlr_total,
      xivusd       TYPE zim01_sol_ap_inv-vl_usd,
      xivusd_      TYPE zim01_sol_ap_inv-vl_usd,
      xdt_ini      TYPE sy-datum,
      xdt_fim      TYPE sy-datum,
      xdtiadto     TYPE sy-datum,
      xdtfadto     TYPE sy-datum,
      xdtiimob     TYPE sy-datum,
      xdtfimob     TYPE sy-datum,
      xdtikob1     TYPE sy-datum,
      xdtfkob1     TYPE sy-datum.

DATA: vdata(10) TYPE c,
      vano(4)   TYPE c,
      xordem    TYPE coas-aufnr,
      vg_1ptela TYPE char1,
      vg_row    TYPE lvc_s_roid-row_id.


DATA: ls_style TYPE lvc_s_styl.
DATA: vg_variant TYPE disvariant,
      vg_repid   LIKE sy-repid,
      wl_display.

DATA: tg_docs_adto_out LIKE TABLE OF t_docs_adto,
      tg_docs_comp_out LIKE TABLE OF t_docs_comp,
      tg_docs_real_out LIKE TABLE OF t_docs_real.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

RANGES: r_ano FOR zim01_sol_ap_inv-ano.

*-CS2022001163-24.03.2023-#103539-JT-inicio
DATA: l_objeto    TYPE imaka-anln1,
      l_row_saida TYPE lvc_index,
      l_dmbtr     TYPE ekbe-dmbtr,
      ok_code     TYPE sy-ucomm,
      l_totitens  TYPE i,
      bot_mvi     TYPE char1,
      bot_mvp     TYPE char1,
      t_sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.

RANGES: r_obj_faixa1 FOR imaka-anln1,
        r_obj_faixa2 FOR imaka-anln1.
*-CS2022001163-24.03.2023-#103539-JT-fim

* US #171475 - MMSILVA - 25.03.2025 - Inicio
TYPES: BEGIN OF ty_anla_sol,
         anln1 type anla-anln1,
         lkauf type anla-lkauf,
         leanz type anla-leanz,
         bukrs type anla-bukrs.
TYPES: END OF ty_anla_sol.

TYPES: BEGIN OF ty_anla_sol_aux,
         lkauf type zim01_sol_ap_inv-vlr_total,
         leanz type imaka-posnr.
TYPES: END OF ty_anla_sol_aux.

DATA: t_anla_sol     TYPE TABLE OF ty_anla_sol,
      w_anla_sol     TYPE ty_anla_sol,
      t_anla_sol_aux TYPE TABLE OF ty_anla_sol_aux,
      w_anla_sol_aux TYPE ty_anla_sol_aux,
      lv_input       TYPE string,
      lv_output      TYPE string.
* US #171475 - MMSILVA - 25.03.2025 - Fim

* US #171475 - MMSILVA - 10.04.2025 - Inicio
DATA: it_aufk TYPE TABLE OF aufk.
* US #171475 - MMSILVA - 10.04.2025 - Fim

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:   p_bukrs FOR zim01_sol_ap_inv-bukrs OBLIGATORY,
  p_werks FOR zim01_sol_ap_inv-gsber,
  p_kostl FOR zim01_sol_ap_inv-kostl,
  p_posnr FOR zim01_sol_ap_inv-posnr,
  p_aufnr FOR coas-aufnr,
  p_data  FOR zim01_sol_ap_inv-dt_inicio  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_varia TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.




IF variante IS INITIAL.
  variante-report = sy-repid.
  variante-variant = p_varia.
ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid         = sy-repid.
  variante-report  = vg_repid.

  IF ( p_varia IS NOT INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.


INITIALIZATION.
  gs_variant_c-report  = sy-repid.

START-OF-SELECTION.

  vg_ac_save = abap_false.

*-CS2022001163-24.03.2023-#103539-JT-inicio
  APPEND VALUE #( sign = 'I' option = 'BT' low = '000000001000'  high = '000000009999' ) TO r_obj_faixa1.
  APPEND VALUE #( sign = 'I' option = 'BT' low = '000000600000'  high = '000000699999' ) TO r_obj_faixa2.
*-CS2022001163-24.03.2023-#103539-JT-fim

  AUTHORITY-CHECK OBJECT 'ZIM14' ID 'Z_AC_SAVE' FIELD 'X'.
  IF sy-subrc EQ 0.
    vg_ac_save = abap_true.
  ENDIF.

  CLEAR: vano, vdata.

  vano = p_data-low+0(4).
  CONCATENATE vano '0101' INTO vdata.

  IF vdata <> p_data-low.
    MESSAGE 'Data Incial deverá ser o primeiro dia do ano do Exercício!' TYPE 'I'.
    EXIT.
  ELSE.
    PERFORM busca_dados.
    PERFORM tratar_dados.
    PERFORM alv.
  ENDIF.

END-OF-SELECTION.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_f4 FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_mod_cells  INTO DATA(wa_good_cells)
          WHERE fieldname EQ 'OBS' OR fieldname EQ 'STATUS'.

      LOOP AT it_saida INTO wa_saida.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.

          WHEN 'STATUS'.
            wa_saida-status   = wa_good_cells-value.
            wa_saida-tabix    = wa_good_cells-row_id.
            MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

*** US - 62758 - CBRAND - Inicio
    LOOP AT er_data_changed->mt_mod_cells  INTO wa_good_cells
    WHERE fieldname EQ 'TEXTO'.

      LOOP AT it_saida INTO wa_saida.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.

          WHEN 'TEXTO'.
            wa_saida-texto    = wa_good_cells-value.
            wa_saida-tabix    = wa_good_cells-row_id.
            MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
*** US - 62758 - CBRAND - Fim

  ENDMETHOD.

  METHOD on_button_click.

    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          ip_mode       TYPE sgs_rwmod.

    CLEAR: st01, st02.

    READ TABLE it_saida INTO wa_saida INDEX es_row_no-row_id.

    CASE es_col_id.
      WHEN 'STATUS'.
        vg_row = es_row_no-row_id.
        vg_1ptela = abap_true.
        CALL SCREEN 0200 STARTING AT 05 05.

      WHEN 'OBSERVACAO'.
        REFRESH  tl_texto.

        IF wa_saida-texto IS NOT INITIAL.
          wl_texto   = wa_saida-texto.
          wl_display = 'X'.
          APPEND wl_texto TO tl_texto.
        ELSE.
          CLEAR: wl_display.
          REFRESH: tl_texto.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Observação'
            im_display_mode = wl_display
          CHANGING
            ch_text         = tl_texto.

        IF wl_display IS INITIAL.
          DATA: wa_linha TYPE cacs_char1000sf.

          CLEAR: wa_texto, wa_linha.

*        " Se tiver mais de uma linha.
          LOOP AT tl_texto INTO wl_texto.
            IF sy-tabix EQ 1.
              wa_linha = wl_texto.
            ELSE.
              CONCATENATE wa_linha wl_texto
              INTO wa_linha SEPARATED BY space.
            ENDIF.
          ENDLOOP.
          wa_saida-texto         = wa_linha.
          "wa_saida-observacao    = '@1E@'. US - 62758 - CBRAND

          MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id .
        ENDIF.

      WHEN 'ANEXO'.

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

        ip_mode = 'E'.

        IF wa_saida-anexo EQ '@02@'.
          vl_ip_service = 'PCATTA_CREA'.
        ELSE.
          vl_ip_service = 'VIEW_ATTA'.
        ENDIF.

        CONCATENATE wa_saida-bukrs wa_saida-gsber wa_saida-posnr  INTO wa_bor-objkey.

        wa_bor-objtype = 'ZIMP60'.

        anexo_obj->set_rw_mode( ip_mode = ip_mode ).

        anexo_obj->start_service_direct(
        EXPORTING
          ip_service         = vl_ip_service
          is_object          = wa_bor
        EXCEPTIONS
          no_object          = 1
          object_invalid     = 2
          execution_failed   = 3
          OTHERS             = 4 ).


        IF sy-subrc EQ 0.
          wa_saida-anexo    = '@FM@'.
        ENDIF.

        MODIFY it_saida FROM wa_saida INDEX es_row_no-row_id .
        CLEAR wa_saida.

        COMMIT WORK.

    ENDCASE.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD catch_hotspot.

    DATA: r_ordem TYPE RANGE OF coas-aufnr,
          vposnr  TYPE imakz-posnr,
          vobjnr  TYPE imakz-objnr,
          vanln1  TYPE imaka-anln1,
          vaufnr  TYPE coas-aufnr. "US #171475 - MMSILVA - 10.04.2025

    DATA: it_rsparams TYPE TABLE OF rsparams,
          wa_rsparams TYPE rsparams.

    CASE e_column_id.
      WHEN 'OBJTO'.
        CLEAR: vobjnr, vposnr.

        READ TABLE it_saida INTO wa_saida INDEX  e_row_id-index.
        CHECK sy-subrc EQ 0.

        vposnr = |{ wa_saida-posnr ALPHA = IN }|.

        READ TABLE it_imakz INTO DATA(wimakz) WITH KEY posnr = vposnr
              objnr = wa_saida-ordem.

* ----> US #171475 - MMSILVA - 10.04.2025 - Inicio
        vaufnr = |{ wa_saida-objto ALPHA = IN }|.

        READ TABLE it_coas INTO wa_coas WITH KEY aufnr = vaufnr.
* ----> US #171475 - MMSILVA - 10.04.2025 - Fim

        IF wimakz IS NOT INITIAL OR wa_coas-user2 IS NOT INITIAL. "US #171475 - MMSILVA - 10.04.2025
          SET PARAMETER ID 'ANR' FIELD wa_saida-objto.
          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
        ENDIF.


        READ TABLE it_imaka INTO wa_imaka WITH KEY posnr = vposnr
        anln1 = wa_saida-ordem.
        IF sy-subrc = 0.
          SET PARAMETER ID 'AN1' FIELD wa_imaka-anln1.
          SET PARAMETER ID 'AN2' FIELD wa_imaka-anln2.
          SET PARAMETER ID 'BUK' FIELD wa_imaka-bukrs.
          CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
        ENDIF.


      WHEN 'XTOAD_BRL' OR 'XTOAD_USD'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        CHECK sy-subrc EQ 0.

        PERFORM f_resumo_documentos_adto USING wa_saida.

      WHEN 'XTBRL_KOB1' OR 'XTUSD_KOB1'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        l_row_saida = e_row_id-index.

        IF ( sy-subrc EQ 0 ) AND ( wa_saida-aufnr IS NOT INITIAL ).

          CLEAR: r_ordem[].
          APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_saida-aufnr ) TO r_ordem.

          PERFORM f_prepare_run_time_info USING abap_true.

          SUBMIT rkaep000  WITH p_tcode   EQ 'KOB1'
          WITH aufnr     IN r_ordem
          WITH p_kokrs   EQ wa_tka02-kokrs
          WITH r_budat   BETWEEN p_data-low AND p_data-high
          WITH p_usedb   = 'X'
          WITH p_usear   = ' '
          WITH p_maxsel  = 200000
          WITH p_disvar  = '/CONFERENCIA'
          AND RETURN.

        ELSE.
*-CS2022001163-24.03.2023-#103539-JT-inicio
          IF wa_saida-objtotipo = 'ATIVO IMOBILIZADO'.
            CALL SCREEN 0300 STARTING AT 60  09
                               ENDING AT 104 09.
          ELSE.
            "Imobilizado.
            PERFORM f_resumo_documentos_real USING wa_saida.
          ENDIF.
*-CS2022001163-24.03.2023-#103539-JT-fim

        ENDIF.
      WHEN 'XTBRL_KOB2' OR 'XTUSD_KOB2'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

*-CS2022001163-24.03.2023-#103539-JT-inicio-JAIME
        IF wa_saida-objtotipo = 'ATIVO IMOBILIZADO'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_saida-objto
            IMPORTING
              output = l_objeto.

          PERFORM f_detalhes_compromisso    USING wa_saida-bukrs
                                                  l_objeto
                                                  wa_saida-anln2.
        ELSE.
*-CS2022001163-24.03.2023-#103539-JT-fim

          CHECK ( sy-subrc EQ 0 ) AND ( wa_saida-aufnr IS NOT INITIAL ).


          CLEAR: r_ordem[].
          APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_saida-aufnr ) TO r_ordem.

          PERFORM f_prepare_run_time_info USING abap_true.

          SUBMIT rkaep000  WITH p_tcode   EQ 'KOB2'
          WITH aufnr     IN r_ordem
          WITH p_kokrs   EQ wa_tka02-kokrs
          WITH r_budat   BETWEEN p_data-low AND '99991231'"p_data-high
          WITH p_usedb   = 'X'
          WITH p_usear   = ' '
          WITH p_maxsel  = 200000
          WITH p_disvar  = '/CONFERENCIA'
          AND RETURN.
        ENDIF.

        "PERFORM F_RESUMO_DOCUMENTOS_COMP USING WA_SAIDA.

      WHEN 'KOSTL_OUT'.

        READ TABLE it_saida INTO wa_saida INDEX  e_row_id-index.
        CHECK sy-subrc EQ 0.

        SET PARAMETER ID 'CAC' FIELD 'MAGI'.
        SET PARAMETER ID 'KOS' FIELD wa_saida-kostl_out.
        CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.

      WHEN 'POSNR'.

        READ TABLE it_saida INTO wa_saida INDEX  e_row_id-index.
        CHECK sy-subrc EQ 0.

        SET PARAMETER ID 'IAF' FIELD wa_saida-posnr. "Ajustado para IAF - MMSILVA - US #171475 - 10.04.2025
        CALL TRANSACTION 'IMA3N' AND SKIP FIRST SCREEN. "Ajustado para IMA3N - MMSILVA - US #171475 - 10.04.2025

    ENDCASE.

  ENDMETHOD.

  METHOD on_f4.

    TYPES: BEGIN OF ty_status,
             status TYPE zim_status,
             desc   TYPE val_text,
           END OF ty_status.

    DATA: lt_map    TYPE TABLE OF dselc,
          ls_map    TYPE dselc,
          lt_return TYPE TABLE OF ddshretval,
          ls_return TYPE ddshretval,
          ls_stable TYPE lvc_s_stbl,

          lt_status TYPE TABLE OF ty_status,
          ls_status TYPE ty_status.

    CASE e_fieldname.
      WHEN 'STATUS'.

        READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<l_out>) INDEX es_row_no-row_id.
        CHECK sy-subrc EQ 0.

        SELECT *
        FROM dd07t INTO TABLE @DATA(tg_dd07t)
              WHERE domname    = 'ZIM_STATUS'
              AND ddlanguage = @sy-langu.

        LOOP AT tg_dd07t INTO DATA(wl_dd07t).
          ls_status-status = wl_dd07t-domvalue_l.
          ls_status-desc   = wl_dd07t-ddtext.
          APPEND ls_status TO lt_status.
        ENDLOOP.

        SORT lt_status BY status.

        "SET RETURN FIELD
        CLEAR ls_map.
        ls_map-fldname = 'F0001'.
        ls_map-dyfldname = 'STATUS'.
        APPEND ls_map TO lt_map.

        " CALL SEARCH HELP POPUP FUNCTION
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'BLART'
            value_org       = 'S'
          TABLES
            value_tab       = lt_status
            dynpfld_mapping = lt_map
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF vg_ac_save EQ abap_true.
          READ TABLE lt_return INTO ls_return WITH KEY fieldname = 'F0001'.
          IF ls_return IS NOT INITIAL.
            <l_out>-status = ls_return-fieldval.
          ENDIF.
        ENDIF.

    ENDCASE.

    ls_stable = ''.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable      = ls_stable
        i_soft_refresh = 'X'
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.

    er_event_data->m_event_handled = 'X'.

  ENDMETHOD.                    "ON_F4


ENDCLASS.



CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    IF vg_ac_save EQ abap_true.
      CLEAR ty_toolbar.
      ty_toolbar-icon      = icon_system_save.
      ty_toolbar-function  = 'SAVE'.
      ty_toolbar-text      = 'Salvar'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'SAVE'.
        PERFORM f_salvar_dados.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND


ENDCLASS.

FORM busca_dados.

  CLEAR: r_ano[].

  DATA: v_ano  TYPE zim01_sol_ap_inv-ano.

*------------------------------------------------------*
* Montar Range Ano
*------------------------------------------------------*

  v_ano = p_data-low(4).
  APPEND VALUE #( sign = 'I' option = 'EQ' low = v_ano ) TO r_ano.

  IF p_data-high IS NOT INITIAL.
    WHILE v_ano < p_data-high(4).
      ADD 1 TO v_ano.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = v_ano ) TO r_ano.
    ENDWHILE.
  ENDIF.

*   US #171475 - MMSILVA - 25.03.2025 - Inicio
  SELECT anln1 lkauf leanz bukrs FROM anla INTO CORRESPONDING FIELDS OF TABLE t_anla_sol
    WHERE zujhr IN r_ano
    AND bukrs IN p_bukrs
    AND zugdt BETWEEN p_data-low AND p_data-high
    AND leanz NE ''.
*   US #171475 - MMSILVA - 25.03.2025 - Fim

* --> US #171475 - MMSILVA - 10.04.2025 - Inicio
    SELECT * FROM aufk INTO TABLE it_aufk
      WHERE aufnr IN p_aufnr
      AND   bukrs IN p_bukrs
      AND   kostv IN p_kostl.
* --> US #171475 - MMSILVA - 10.04.2025 - Fim

  SELECT  bukrs  gsber  ano  kostl  posnr  descr_item  vlr_total vl_usd solicitacao_invest "US #171475 - MMSILVA - 25.03.2025 - Acrescentado "SOLICITACAO_INVEST"
  FROM zim01_sol_ap_inv INTO CORRESPONDING FIELDS OF TABLE it_zim01_aux
  WHERE  bukrs IN p_bukrs
  AND    ano   IN r_ano
  AND    gsber IN p_werks
  AND    kostl IN p_kostl.
*  AND    posnr NE ''. "US #171475 - MMSILVA - 25.03.2025 - Comentado devido atualização SYSPHERA.

  LOOP AT it_zim01_aux INTO DATA(wa_zim).

*   US #171475 - MMSILVA - 10.04.2025 - Inicio
    IF wa_zim-solicitacao_invest IS NOT INITIAL.
      READ TABLE t_anla_sol INTO w_anla_sol WITH KEY leanz = wa_zim-solicitacao_invest.
    ENDIF.
*   US #171475 - MMSILVA - 10.04.2025 - Fim

    wa_zim01-posnr              = |{ wa_zim-posnr  ALPHA = IN }|.
    wa_zim01-posnr_out          = |{ wa_zim-posnr  ALPHA = OUT }|.
    wa_zim01-bukrs              = wa_zim-bukrs.
    wa_zim01-gsber              = wa_zim-gsber.
    wa_zim01-ano                = wa_zim-ano.
    wa_zim01-safra              = wa_zim-safra.
    wa_zim01-safra2             = wa_zim-safra2.
    wa_zim01-kostl              = wa_zim-kostl.
    wa_zim01-buzei              = wa_zim-buzei.
    wa_zim01-descr_item         = wa_zim-descr_item.
    wa_zim01-vlr_total          = wa_zim-vlr_total.
    wa_zim01-vl_usd             = wa_zim-vl_usd.
*   US #171475 - MMSILVA - 25.03.2025 - Inicio
    IF wa_zim-posnr IS INITIAL AND |{ wa_zim-solicitacao_invest ALPHA = IN }| CO '1234567890'.
      wa_zim01-solicitacao_invest = |{ wa_zim-solicitacao_invest ALPHA = IN }|.
    ENDIF.

    IF w_anla_sol-anln1 IS NOT INITIAL.
      wa_zim01-anln1 = w_anla_sol-anln1.
    ENDIF.

    IF wa_zim01-posnr IS INITIAL AND wa_zim01-solicitacao_invest IS NOT INITIAL.
      wa_zim01-vlr_total = w_anla_sol-lkauf.
    ENDIF.
*   US #171475 - MMSILVA - 25.03.2025 - Fim

    APPEND wa_zim01  TO it_zim01.
    CLEAR: wa_zim, wa_zim01_sol_ap_inv, w_anla_sol, wa_zim01.

  ENDLOOP.

  IF p_posnr IS NOT INITIAL.
    LOOP AT  it_zim01 INTO wa_zim01 WHERE posnr IN p_posnr.
      MOVE-CORRESPONDING wa_zim01 TO wa_zim01_sol_ap_inv.
      APPEND wa_zim01_sol_ap_inv TO it_zim01_sol_ap_inv.
      CLEAR: wa_zim01, wa_zim01_sol_ap_inv.
    ENDLOOP.
  ELSE.
    it_zim01_sol_ap_inv[] = it_zim01[].
  ENDIF.

  SORT it_zim01_sol_ap_inv BY posnr ano.
*  DELETE ADJACENT DUPLICATES FROM it_zim01_sol_ap_inv COMPARING posnr. "US #171475 - MMSILVA - 25.03.2025 - Comentado devido atualização SYSPHERA.

  IF it_zim01_sol_ap_inv[] IS NOT INITIAL.

    CLEAR: it_zim01[].

    SELECT *
    FROM zim01_sol_ap_inv APPENDING CORRESPONDING FIELDS OF TABLE it_zim01
    FOR ALL ENTRIES IN it_zim01_sol_ap_inv
    WHERE posnr EQ it_zim01_sol_ap_inv-posnr.

    SELECT *
    FROM zim01_sol_ap_inv APPENDING CORRESPONDING FIELDS OF TABLE it_zim01
    FOR ALL ENTRIES IN it_zim01_sol_ap_inv
    WHERE posnr EQ it_zim01_sol_ap_inv-posnr_out.

    LOOP AT it_zim01 ASSIGNING FIELD-SYMBOL(<fs_zim01>).
      <fs_zim01>-posnr = |{ <fs_zim01>-posnr  ALPHA = IN }|.
    ENDLOOP.

    SORT it_zim01 BY                                   bukrs gsber ano safra safra2 kostl buzei.
    DELETE ADJACENT DUPLICATES FROM it_zim01 COMPARING bukrs gsber ano safra safra2 kostl buzei.

    "Get Exercio Fim
    SORT r_ano BY low.
    DATA(index_fim) = lines( r_ano[] ).
    READ TABLE r_ano INDEX index_fim.
    DATA(ano_fim) = r_ano-low.

    LOOP AT it_zim01 WHERE ano > ano_fim.
      DELETE it_zim01.
    ENDLOOP.

    "Subistituir Tabela Saida por registro de menor ano
    it_zim01_ano_min[] = it_zim01[].
    SORT it_zim01_ano_min BY posnr ano.
    DELETE ADJACENT DUPLICATES FROM it_zim01_ano_min COMPARING posnr solicitacao_invest. "US #171475 - MMSILVA - 25.03.2025 - Acrescentado "Solicitacao_invest"

    LOOP AT it_zim01_ano_min.
      LOOP AT it_zim01_sol_ap_inv ASSIGNING FIELD-SYMBOL(<fs_sol_inv>) WHERE posnr = it_zim01_ano_min-posnr
      AND ano   > it_zim01_ano_min-ano AND solicitacao_invest = it_zim01_ano_min-solicitacao_invest. "US #171475 - MMSILVA - 26.03.2025 - Acrescentado "solicitacao_invest"
        CLEAR: <fs_sol_inv>.
        MOVE-CORRESPONDING it_zim01_ano_min TO <fs_sol_inv>.
      ENDLOOP.
    ENDLOOP.

    SELECT  * FROM tka02 INTO TABLE it_tka02
    FOR ALL ENTRIES IN it_zim01_sol_ap_inv
    WHERE  bukrs EQ  it_zim01_sol_ap_inv-bukrs.

    READ TABLE it_tka02 INTO wa_tka02 INDEX 1.
    IF sy-subrc = 0.

      SELECT  * FROM cskt INTO TABLE it_cskt
      FOR ALL ENTRIES IN it_zim01_sol_ap_inv
      WHERE spras EQ sy-langu
      AND   kokrs EQ wa_tka02-kokrs
      AND   kostl EQ it_zim01_sol_ap_inv-kostl
      AND   datbi EQ '99991231'.


      SELECT  * FROM imakz INTO TABLE it_imakz
      FOR ALL ENTRIES IN it_zim01_sol_ap_inv
      WHERE posnr EQ it_zim01_sol_ap_inv-posnr.

      SELECT * FROM  imaka INTO TABLE it_imaka
      FOR ALL ENTRIES IN it_zim01_sol_ap_inv
      WHERE ( posnr EQ it_zim01_sol_ap_inv-posnr OR anln1 EQ it_zim01_sol_ap_inv-anln1 ) "US #171475 - MMSILVA - 25.03.2025 - Acrescentado ANLN1
      AND   ( anln1 NOT LIKE 'OBR%' OR  anln1 NOT LIKE 'EQP%' ) .

      LOOP AT it_imakz INTO wa_imakz.
        w_ordem-sign   = 'I'.
        w_ordem-option = 'EQ'.
        w_ordem-low    = wa_imakz-objnr+2(12).
        w_ordem-high   = ''.

        IF w_ordem-low IN p_aufnr.
          APPEND w_ordem TO it_ordem.
        ENDIF.

        wa_ordem-posnr = wa_imakz-posnr.
        wa_ordem-objnr = wa_imakz-objnr.

* ----> US #171475 - MMSILVA - 10.04.2025 - Inicio
        SELECT SINGLE * FROM aufk INTO @DATA(w_aufk) WHERE objnr = @wa_ordem-objnr.
        IF sy-subrc IS INITIAL.
          wa_ordem-bukrs = w_aufk-bukrs.
        ENDIF.
* ----> US #171475 - MMSILVA - 10.04.2025 - FIM


        APPEND wa_ordem TO t_ordem.

        CLEAR: w_ordem, wa_imakz, wa_ordem.
      ENDLOOP.


      LOOP AT it_imaka INTO wa_imaka.

        wa_ordem-posnr  = wa_imaka-posnr.
        wa_ordem-objnr  = wa_imaka-anln1.
        wa_ordem-lfdnr  = wa_imaka-lfdnr.
        wa_ordem-bukrs  = wa_imaka-bukrs.
        wa_ordem-anln2  = wa_imaka-anln2.
        wa_ordem-equnr  = wa_imaka-equnr.

        APPEND wa_ordem TO t_ordem.
        CLEAR: wa_ordem.

        w_ordem-sign   = 'I'.
        w_ordem-option = 'EQ'.
        w_ordem-low    = wa_imaka-anln1.
        w_ordem-high   = ''.

        IF p_aufnr IS NOT INITIAL AND w_ordem-low IS NOT INITIAL.
          IF w_ordem-low IN p_aufnr.
            APPEND w_ordem TO it_ordem.
          ENDIF.
        ENDIF.
        CLEAR: wa_ordem, wa_imaka.
      ENDLOOP.

*     US #171475 - MMSILVA - 25.03.2025 - Inicio
      "Acrescenta materiais que não possuem posnr, apenas sol. sysphera
      LOOP AT it_zim01_sol_ap_inv INTO wa_zim01_sol_ap_inv.
        IF wa_zim01_sol_ap_inv-posnr IS INITIAL.
          READ TABLE t_anla_sol INTO w_anla_sol WITH KEY anln1 = wa_zim01_sol_ap_inv-anln1.
          IF sy-subrc IS INITIAL.
            wa_ordem-objnr              = w_anla_sol-anln1.
            wa_ordem-bukrs              = w_anla_sol-bukrs.
            wa_ordem-solicitacao_invest = wa_zim01_sol_ap_inv-solicitacao_invest.

            APPEND wa_ordem TO t_ordem.
            CLEAR: wa_zim01_sol_ap_inv, wa_ordem, w_anla_sol.
          ENDIF.
        ENDIF.
      ENDLOOP.

* --> US #171475 - MMSILVA - 10.04.2025 - Inicio
      SELECT aufnr  ktext  user2 bukrs FROM coas INTO TABLE it_coas FOR ALL ENTRIES IN it_aufk WHERE aufnr EQ it_aufk-aufnr AND kostv IN p_kostl.
      LOOP AT it_coas INTO wa_coas.
        IF wa_coas-user2 IS NOT INITIAL AND |{ wa_coas-user2 ALPHA = IN }| CO '1234567890'.
          wa_ordem-objnr              = wa_coas-aufnr.
          CONCATENATE 'OR' wa_ordem-objnr INTO wa_ordem-objnr.
          wa_ordem-bukrs              = wa_coas-bukrs.
          wa_ordem-solicitacao_invest = wa_coas-user2.
          APPEND wa_ordem TO t_ordem.

          w_ordem-sign   = 'I'.
          w_ordem-option = 'EQ'.
          w_ordem-low    = wa_coas-aufnr.
          w_ordem-high   = ''.
          APPEND w_ordem TO it_ordem.

          CLEAR: wa_coas, wa_ordem.
        ENDIF.
      ENDLOOP.

      FREE: it_coas.
* --> US #171475 - MMSILVA - 10.04.2025 - Fim

      "Remove duplicados caso o material tenha sido registrado em uma sol. de investimento e não possui sol. sysphera
      LOOP AT t_ordem INTO wa_ordem.
        LOOP AT t_ordem INTO DATA(wa_ordem_dup) WHERE objnr = wa_ordem-objnr AND bukrs = wa_ordem-bukrs.
          IF wa_ordem_dup-posnr IS INITIAL AND wa_ordem_dup NE wa_ordem.
            DELETE t_ordem INDEX sy-tabix.
            EXIT.
          ENDIF.
        ENDLOOP.

        CLEAR: wa_ordem.
      ENDLOOP.

      LOOP AT t_ordem INTO wa_ordem.
        READ TABLE it_imaka INTO wa_imaka WITH KEY anln1 = wa_ordem-objnr.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: wa_imaka.
          wa_imaka-anln1 = wa_ordem-objnr.
          wa_imaka-bukrs = wa_ordem-bukrs.
          wa_imaka-anln2 = '0000'.
          APPEND wa_imaka TO it_imaka.
        ENDIF.
        CLEAR: wa_imaka, wa_ordem.
      ENDLOOP.

      SORT t_ordem BY objnr bukrs.
      DELETE ADJACENT DUPLICATES FROM t_ordem COMPARING objnr bukrs.
*     US #171475 - MMSILVA - 25.03.2025 - Fim

      IF it_ordem[] IS NOT INITIAL.

        SELECT aufnr  ktext  user2 bukrs FROM coas INTO TABLE it_coas "US #171475 - MMSILVA - 10.04.2025 - Acrescentado USER2 e BUKRS
        WHERE aufnr IN it_ordem.

        PERFORM busca_dados_kob1.
        PERFORM busca_dados_kob2.
      ENDIF.

      IF it_imaka[] IS NOT INITIAL.

        SELECT   bukrs anln1 anln2 txt50 lkauf aibn1 deakt
        FROM anla INTO TABLE it_anla
        FOR ALL ENTRIES IN it_imaka
        WHERE bukrs EQ  p_bukrs-low
        AND   anln1 EQ  it_imaka-anln1
        AND   anln2  EQ  it_imaka-anln2.


        SELECT  bukrs  anln1  anln2  lnran afabe  bwasl  bzdat   anbtr
        FROM anep INTO TABLE it_anep
        FOR ALL ENTRIES IN it_imaka
        WHERE  bukrs   EQ p_bukrs-low
        AND  anln1   EQ it_imaka-anln1
        AND  anln2   EQ it_imaka-anln2
        AND  afabe   EQ '01'
        "            AND  BWASL   IN ('100','105')
        AND  bzdat   IN p_data.


        SELECT  bukrs  anln1  anln2 lnran afabe  bwasl  bzdat   anbtr
        FROM anep INTO TABLE it_anep_usd
        FOR ALL ENTRIES IN it_imaka
        WHERE  bukrs   EQ p_bukrs-low
        AND  anln1   EQ it_imaka-anln1
        AND  anln2   EQ it_imaka-anln2
        AND  afabe   EQ '41'
        "            AND  BWASL   IN ('100','105')
        AND  bzdat   IN p_data.


*        SELECT  AUFNR  ANLN1 ANLN2  EBELN  EBELP
*           FROM EKKN AS A INTO TABLE IT_EKKN
*          FOR ALL ENTRIES IN IT_IMAKA
*          WHERE "AUFNR  IN IT_ORDEM.
*              ANLN1  EQ IT_IMAKA-ANLN1
*        AND   ANLN2  EQ IT_IMAKA-ANLN2
*        AND EXISTS ( SELECT EBELN
*                       FROM EKPO AS B
*                      WHERE B~EBELN = A~EBELN
*                        AND B~EBELP = A~EBELP
*                        AND B~LOEKZ = SPACE ).
        IF it_coas IS NOT INITIAL.
          SORT it_coas BY aufnr.

          "Ordem.
          IF it_ordem IS NOT INITIAL.
            SELECT aufnr  anln1 anln2  ebeln  ebelp
            FROM ekkn INTO TABLE it_ekkn
            FOR ALL ENTRIES IN it_coas
            WHERE aufnr  IN it_ordem
            AND aufnr  EQ it_coas-aufnr.
          ELSE.

            SELECT aufnr  anln1 anln2  ebeln  ebelp
            FROM ekkn INTO TABLE it_ekkn
            FOR ALL ENTRIES IN it_coas
            WHERE aufnr  EQ it_coas-aufnr.
          ENDIF.
        ENDIF.

        "Imobilizado.
        IF it_imaka IS NOT INITIAL.
          SELECT aufnr  anln1 anln2  ebeln  ebelp
          FROM ekkn
          APPENDING TABLE it_ekkn
          FOR ALL ENTRIES IN it_imaka
          WHERE anln1  EQ it_imaka-anln1.
        ENDIF.

        IF it_ekkn[] IS NOT INITIAL.

          SORT it_ekkn BY ebeln ebelp.
          SELECT ebeln  ebelp  vgabe  belnr buzei gjahr
          FROM ekbe INTO TABLE it_ekbe
          FOR ALL ENTRIES IN it_ekkn
          WHERE ebeln  EQ it_ekkn-ebeln
          AND ebelp   EQ it_ekkn-ebelp
          AND budat   IN p_data
          AND vgabe  IN ('2', '4', 'C').

          LOOP AT it_ekbe ASSIGNING FIELD-SYMBOL(<fs_ekbe>).
            <fs_ekbe>-buzei_fi = <fs_ekbe>-buzei+1(3).
          ENDLOOP.

          IF it_ekbe[] IS NOT INITIAL.

            SELECT bukrs belnr buzei gjahr  budat
            shkzg dmbtr  dmbe2
            FROM bsik INTO TABLE it_bsik
            FOR ALL ENTRIES IN it_ekbe
            WHERE bukrs  EQ p_bukrs-low
            AND belnr  EQ  it_ekbe-belnr
            AND gjahr  EQ  it_ekbe-gjahr
            AND buzei  EQ  it_ekbe-buzei_fi.

            SELECT bukrs belnr  buzei gjahr budat shkzg dmbtr dmbe2
            FROM bsak INTO TABLE it_bsak
            FOR ALL ENTRIES IN it_ekbe
            WHERE bukrs EQ  p_bukrs-low
            AND belnr EQ  it_ekbe-belnr
            AND gjahr EQ  it_ekbe-gjahr
            AND buzei EQ  it_ekbe-buzei_fi.

            it_bkpf_aux = VALUE #( FOR l  IN it_ekbe ( buzei_fi = l-buzei_fi belnr = l-belnr gjahr = l-gjahr awkey = |{ l-belnr }{ l-gjahr }| ) ).

            IF it_bkpf_aux IS NOT INITIAL.
              SELECT * FROM bkpf INTO TABLE it_bkpf
              FOR ALL ENTRIES IN it_bkpf_aux
              WHERE awkey EQ it_bkpf_aux-awkey.

              IF it_bkpf IS NOT INITIAL.

                SELECT bukrs belnr buzei gjahr  budat
                shkzg dmbtr  dmbe2
                FROM bsik APPENDING TABLE it_bsik
                FOR ALL ENTRIES IN it_bkpf
                WHERE bukrs   EQ it_bkpf-bukrs
                AND belnr     EQ  it_bkpf-belnr
                AND gjahr     EQ  it_bkpf-gjahr.

                SELECT bukrs belnr  buzei gjahr budat shkzg dmbtr dmbe2
                FROM bsak APPENDING TABLE it_bsak
                FOR ALL ENTRIES IN it_bkpf
                WHERE bukrs EQ it_bkpf-bukrs
                AND belnr   EQ  it_bkpf-belnr
                AND gjahr   EQ  it_bkpf-gjahr.
              ENDIF.

            ENDIF.


          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


FORM tratar_dados.

  DATA: xdtinicial TYPE sy-datum,
        xdtfinal   TYPE sy-datum.

  DATA: wa_doc_adto LIKE t_docs_adto,
        wa_doc_comp LIKE t_docs_comp,
        vg_anln1    TYPE anln1,
        wa_doc_real LIKE t_docs_real.

  DATA: wl_name    TYPE thead-tdname,
        vl_obj_key TYPE sibflporb-instid,
        vl_lines   TYPE i,
        ls_aufnr   TYPE aufnr,
        anexos     TYPE TABLE OF bdn_con.

  DATA: lva_dmbtr TYPE ekbe-dmbtr.

  LOOP AT t_ordem INTO wa_ordem.
*   US #171475 - MMSILVA - 25.03.2025 - Inicio
    IF wa_ordem-posnr IS NOT INITIAL.
      READ TABLE it_zim01_sol_ap_inv INTO  wa_zim01_sol_ap_inv WITH KEY posnr = |{ wa_ordem-posnr ALPHA = IN }|.
    ELSE.
      READ TABLE it_zim01_sol_ap_inv INTO  wa_zim01_sol_ap_inv WITH KEY solicitacao_invest = |{ wa_ordem-solicitacao_invest ALPHA = IN }|.
    ENDIF.

    CHECK wa_zim01_sol_ap_inv IS NOT INITIAL.
*   US #171475 - MMSILVA - 25.03.2025 - Fim

    CLEAR: wa_saida, xdt_ini, xdt_fim.
    wa_saida-objnr              = wa_ordem-objnr.
    wa_saida-objto              = |{ wa_ordem-objnr ALPHA = OUT }|. "US #171475 - MMSILVA - 25.03.2025
    wa_saida-bukrs              = wa_zim01_sol_ap_inv-bukrs.
    wa_saida-gsber              = wa_zim01_sol_ap_inv-gsber.
    wa_saida-ano                = wa_zim01_sol_ap_inv-ano.
    wa_saida-posnr              = |{ wa_zim01_sol_ap_inv-posnr ALPHA = OUT }|.
    wa_saida-kostl              = wa_zim01_sol_ap_inv-kostl.
    wa_saida-kostl_out          = |{ wa_zim01_sol_ap_inv-kostl ALPHA = OUT }|.
    wa_saida-descr_item         = wa_zim01_sol_ap_inv-descr_item.
*   US #171475 - MMSILVA - 14.04.2025 - Inicio
    IF wa_saida-posnr IS INITIAL AND |{ wa_ordem-solicitacao_invest ALPHA = IN }| CO '1234567890'.
      wa_saida-solicitacao_invest = wa_ordem-solicitacao_invest.
    ENDIF.
*   US #171475 - MMSILVA - 14.04.2025 - Fim

* US - 62758 - Fim- CBRAND

    READ TABLE it_tka02 INTO wa_tka02 WITH KEY bukrs  = wa_zim01_sol_ap_inv-bukrs.

    READ TABLE it_cskt INTO wa_cskt WITH KEY kostl  = wa_zim01_sol_ap_inv-kostl
    kokrs  = wa_tka02-kokrs.
    IF sy-subrc = 0.
      wa_saida-ltext = wa_cskt-ltext.
    ENDIF.

    LOOP AT it_zim01 INTO wa_zim01 WHERE bukrs  = wa_zim01_sol_ap_inv-bukrs
    AND posnr =  wa_zim01_sol_ap_inv-posnr
    AND solicitacao_invest = wa_zim01_sol_ap_inv-solicitacao_invest. "US #171475 - MMSILVA - 25.03.2025
      xivbrl = xivbrl + wa_zim01-vlr_total.
      xivusd = xivusd + wa_zim01-vl_usd.
      CLEAR wa_zim01.
    ENDLOOP.


    READ TABLE it_imaka INTO wa_imaka WITH KEY posnr  = wa_ordem-posnr
    anln1  = wa_ordem-objnr.
    IF sy-subrc = 0.


      wa_saida-objto  =  |{ wa_ordem-objnr ALPHA = OUT  }|.

      wa_saida-ordem  = wa_ordem-objnr.
      wa_saida-anln2  = wa_imaka-anln2.


      READ TABLE it_anla INTO wa_anla WITH KEY bukrs  = wa_zim01_sol_ap_inv-bukrs
      anln1  = wa_imaka-anln1
      anln2  = wa_imaka-anln2.

      IF sy-subrc = 0.
        wa_saida-xivbrl = wa_anla-lkauf.
        wa_saida-desc_objto = wa_anla-txt50.
        wa_saida-aibn1 = wa_anla-aibn1. "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
        wa_saida-deakt = wa_anla-deakt. "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
      ENDIF.


      SORT it_anep BY anln1 bzdat.
      LOOP AT it_anep INTO wa_anep WHERE  bukrs = wa_zim01_sol_ap_inv-bukrs AND
      anln1 = wa_imaka-anln1 AND
      anln2 = wa_imaka-anln2.
        xdtinicial  = wa_anep-bzdat.
        xdtfinal    = wa_anep-bzdat.

        IF xdt_ini IS INITIAL.
          xdt_ini = xdtinicial.
        ELSE.
          IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
            xdt_ini = xdtinicial.
          ENDIF.
        ENDIF.

        xtotbrl_imob = xtotbrl_imob + wa_anep-anbtr.
        CLEAR wa_anep.
      ENDLOOP.


      LOOP AT it_anep_usd INTO wa_anep_usd WHERE  bukrs = wa_zim01_sol_ap_inv-bukrs AND
      anln1 = wa_imaka-anln1 AND
      anln2 = wa_imaka-anln2.
        xtotusd_imob = xtotusd_imob + wa_anep_usd-anbtr.
        CLEAR wa_anep.
      ENDLOOP.

      CLEAR:  xdtinicial, xdtfinal.
    ENDIF.

*      LOOP AT IT_EKKN INTO WA_EKKN WHERE ANLN1 EQ WA_IMAKA-ANLN1
*                                     AND ANLN2 EQ WA_IMAKA-ANLN2.

    LOOP AT it_ekkn INTO wa_ekkn WHERE aufnr EQ wa_ordem-objnr+2(12).


      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln  = wa_ekkn-ebeln
      AND ebelp  = wa_ekkn-ebelp.

        CLEAR: wa_doc_adto.

        wa_doc_adto-bukrs = wa_zim01_sol_ap_inv-bukrs.
        wa_doc_adto-ebeln = wa_ekbe-ebeln.
        wa_doc_adto-ebelp = wa_ekbe-ebelp.
        wa_doc_adto-vgabe = wa_ekbe-vgabe.

        CASE wa_doc_adto-vgabe.
          WHEN '4'.
            wa_doc_adto-ds_operacao = 'Adiantamento'.
          WHEN 'C'.
            wa_doc_adto-ds_operacao = 'Compensação Adiantamento'.
        ENDCASE.

        LOOP AT it_bsik INTO wa_bsik WHERE bukrs  = wa_zim01_sol_ap_inv-bukrs  AND
        belnr  = wa_ekbe-belnr AND
        gjahr  = wa_ekbe-gjahr AND
        buzei  = wa_ekbe-buzei_fi.

          wa_doc_adto-belnr = wa_bsik-belnr.
          wa_doc_adto-gjahr = wa_bsik-gjahr.
          wa_doc_adto-buzei = wa_bsik-buzei.
          wa_doc_adto-budat = wa_bsik-budat.

          IF wa_bsik-shkzg = 'H'.
            xtoad_brl =  xtoad_brl + ( wa_bsik-dmbtr  * - 1 ).
            xtoad_usd =  xtoad_usd + ( wa_bsik-dmbe2  * - 1 ).

            wa_doc_adto-dmbtr = ( wa_bsik-dmbtr  * - 1 ).
            wa_doc_adto-dmbe2 = ( wa_bsik-dmbe2  * - 1 ).

          ELSE.
            xtoad_brl =  xtoad_brl + wa_bsik-dmbtr.
            xtoad_usd =  xtoad_usd + wa_bsik-dmbe2.

            wa_doc_adto-dmbtr = wa_bsik-dmbtr.
            wa_doc_adto-dmbe2 = wa_bsik-dmbe2.
          ENDIF.

          APPEND wa_doc_adto TO wa_saida-docs_adto.

          xdtinicial =  wa_bsik-budat.
          xdtfinal   =  wa_bsik-budat.

          IF xdt_ini IS INITIAL.
            xdt_ini = xdtinicial.
          ELSE.
            IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
              xdt_ini = xdtinicial.
            ENDIF.
          ENDIF.

        ENDLOOP.

        LOOP AT it_bsak INTO wa_bsak WHERE bukrs  = wa_zim01_sol_ap_inv-bukrs  AND
        belnr  = wa_ekbe-belnr AND
        gjahr  = wa_ekbe-gjahr AND
        buzei  = wa_ekbe-buzei_fi.

          wa_doc_adto-belnr = wa_bsak-belnr.
          wa_doc_adto-gjahr = wa_bsak-gjahr.
          wa_doc_adto-buzei = wa_bsak-buzei.
          wa_doc_adto-budat = wa_bsak-budat.

          IF wa_bsak-shkzg = 'H'.
            xtoad_brl =  xtoad_brl + ( wa_bsak-dmbtr  * - 1 ).
            xtoad_usd =  xtoad_usd + ( wa_bsak-dmbe2  * - 1 ).

            wa_doc_adto-dmbtr = ( wa_bsak-dmbtr  * - 1 ).
            wa_doc_adto-dmbe2 = ( wa_bsak-dmbe2  * - 1 ).

          ELSE.
            xtoad_brl =  xtoad_brl + wa_bsak-dmbtr.
            xtoad_usd =  xtoad_usd + wa_bsak-dmbe2.

            wa_doc_adto-dmbtr = wa_bsak-dmbtr.
            wa_doc_adto-dmbe2 = wa_bsak-dmbe2.
          ENDIF.

          APPEND wa_doc_adto TO wa_saida-docs_adto.

          xdtinicial =  wa_bsak-budat.
          xdtfinal   =  wa_bsak-budat.

          IF xdt_ini IS INITIAL.
            xdt_ini = xdtinicial.
          ELSE.
            IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
              xdt_ini = xdtinicial.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDLOOP. "LOOP AT IT_EKBE
    ENDLOOP.


    "READ TABLE IT_IMAKA --> IF SY-SUBRC = 0


    "ORDEM
    CLEAR:  xdtinicial,   xdtfinal.

    READ TABLE it_imakz INTO wa_imakz WITH KEY posnr = wa_ordem-posnr
                                               objnr = wa_ordem-objnr.

    READ TABLE it_coas INTO wa_coas WITH KEY aufnr = wa_ordem-objnr+2(12).

    IF ( wa_imakz IS NOT INITIAL OR wa_coas-user2 IS NOT INITIAL ). "US #171475 - MMSILVA - 10.04.2025

      CLEAR xordem.
      xordem  = wa_imakz-objnr+2(12).
* --> US #171475 - MMSILVA - 10.04.2025 - Inicio
      IF xordem IS INITIAL.
        xordem = wa_ordem-objnr+2(12).
      ENDIF.
* --> US #171475 - MMSILVA - 10.04.2025 - Fim

      READ TABLE it_coas INTO wa_coas WITH KEY aufnr = xordem.
      IF sy-subrc = 0.
        wa_saida-aufnr      =  wa_coas-aufnr.
        wa_saida-desc_objto =  wa_coas-ktext.
        wa_saida-objto      =  |{ xordem ALPHA = OUT  }|.
        CONCATENATE 'OR' xordem INTO   wa_saida-ordem.
* ----> US #171475 - MMSILVA - 10.04.2025 - Inicio
        CONDENSE wa_coas-user2 NO-GAPS.
        IF wa_saida-posnr IS INITIAL AND |{ wa_coas-user2 ALPHA = IN }| CO '1234567890'.
          wa_saida-solicitacao_invest = wa_coas-user2.
        ENDIF.
* ----> US #171475 - MMSILVA - 10.04.2025 - Fim
      ENDIF.

      CLEAR:  xtbrl_kob1, xtusd_kob1, xdtikob1, xdtfkob1,  xtbrl_kob2, xtusd_kob2.

      SORT it_kob1 BY aufnr budat.

      LOOP AT it_kob1 INTO wa_kob1 WHERE aufnr = wa_coas-aufnr.

        xdtinicial  = wa_kob1-budat.
        xdtfinal    = wa_kob1-budat.

        CLEAR wa_doc_real.

        wa_doc_real-aufnr         = wa_kob1-aufnr.
        wa_doc_real-budat         = wa_kob1-budat.
        wa_doc_real-wogbtr        = wa_kob1-wogbtr.
        wa_doc_real-wkgbtr        = wa_kob1-wkgbtr.
        wa_doc_real-ebeln         = wa_kob1-ebeln.
        wa_doc_real-ebelp         = wa_kob1-ebelp.
        wa_doc_real-bukrs         = wa_kob1-bukrs.
        wa_doc_real-belnr_fi      = wa_kob1-belnr_fi.
        wa_doc_real-gjahr         = wa_kob1-gjahr.
        wa_doc_real-buzei         = wa_kob1-buzei.
        wa_doc_real-refbn         = wa_kob1-refbn.
        wa_doc_real-refgj         = wa_kob1-refgj.
        wa_doc_real-document_key  = wa_kob1-document_key.




        APPEND wa_doc_real TO wa_saida-docs_real.

        IF xdt_ini IS INITIAL.
          xdt_ini = xdtinicial.
        ELSE.
          IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
            xdt_ini = xdtinicial.
          ENDIF.
        ENDIF.

        IF xdt_fim IS INITIAL.
          xdt_fim = xdtfinal.
        ELSE.
          IF ( xdtfinal > xdt_fim ) AND ( xdtfinal IS NOT INITIAL ).
            xdt_fim = xdtfinal.
          ENDIF.
        ENDIF.

        xtbrl_kob1 =  xtbrl_kob1 + wa_kob1-wogbtr.
        xtusd_kob1 =  xtusd_kob1 + wa_kob1-wkgbtr.
      ENDLOOP.


      LOOP AT it_kob2 INTO wa_kob2
      WHERE aufnr = wa_coas-aufnr.
        xdtinicial  = wa_kob2-budat.
        xdtfinal    = wa_kob2-budat.

        CLEAR wa_doc_comp.

        wa_doc_comp-aufnr         = wa_kob2-aufnr.
        wa_doc_comp-budat         = wa_kob2-budat.
        wa_doc_comp-wogbtr        = wa_kob2-wogbtr.
        wa_doc_comp-wkgbtr        = wa_kob2-wkgbtr.
        wa_doc_comp-ebeln         = wa_kob2-ebeln.
        wa_doc_comp-ebelp         = wa_kob2-ebelp.
        wa_doc_comp-bukrs         = wa_kob2-bukrs.
        wa_doc_comp-belnr_fi      = wa_kob2-belnr_fi.
        wa_doc_comp-gjahr         = wa_kob2-gjahr.
        wa_doc_comp-buzei         = wa_kob2-buzei.
        wa_doc_comp-refbn         = wa_kob2-refbn.
        wa_doc_comp-refgj         = wa_kob2-refgj.
        wa_doc_comp-document_key  = wa_kob2-document_key.

        APPEND wa_doc_comp TO wa_saida-docs_comp.

        IF xdt_ini IS INITIAL.
          xdt_ini = xdtinicial.
        ELSE.
          IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
            xdt_ini = xdtinicial.
          ENDIF.
        ENDIF.

        IF xdt_fim IS INITIAL.
          xdt_fim = xdtfinal.
        ELSE.
          IF ( xdtfinal > xdt_fim ) AND ( xdtfinal IS NOT INITIAL ).
            xdt_fim = xdtfinal.
          ENDIF.
        ENDIF.

        xtbrl_kob2 = xtbrl_kob2 + wa_kob2-wogbtr.
        xtusd_kob2 = xtusd_kob2 + wa_kob2-wkgbtr.
      ENDLOOP.

      wa_saida-xtbrl_kob1 = xtbrl_kob1.
      wa_saida-xtusd_kob1 = xtusd_kob1.
      wa_saida-xtbrl_kob2 = xtbrl_kob2.
      wa_saida-xtusd_kob2 = xtusd_kob2.
    ENDIF.

    wa_saida-dt_inicio  =  xdt_ini.

    IF wa_saida-dt_fim IS NOT INITIAL.
      xdt_fim = wa_saida-dt_fim.
    ENDIF.

    IF xdt_fim IS NOT INITIAL.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = xdt_fim
          i_date_to   = sy-datum
        IMPORTING
          e_days      = wa_saida-dias_s_mov.
    ELSE.
      wa_saida-dias_s_mov = 0.
    ENDIF.
* US - 62758 - CBRAND - Inicio
    SELECT * FROM bpja INTO TABLE @DATA(t_bpja) "#EC CI_DB_OPERATION_OK[2226072]
          WHERE objnr EQ @wa_saida-ordem "#EC CI_DB_OPERATION_OK[2226048]
          AND wrttp EQ '42'.

    IF sy-subrc EQ 0.
      "CS2021000657  / Anderson Oenning

      LOOP AT t_bpja INTO DATA(w_bpja).
        ADD w_bpja-wtjhv TO wa_saida-xivbrl.
        ADD w_bpja-wtjhv TO wa_saida-xivusd.
      ENDLOOP.

      "CS2021000657  / Anderson Oenning

*      IF wa_saida-xivusd IS INITIAL AND xivusd IS NOT INITIAL.
      IF xivusd IS NOT INITIAL. "RJF
        wa_saida-xivusd = xivusd.
      ENDIF.

      wa_saida-xtoad_brl  = xtoad_brl.
      wa_saida-xtoad_usd  = xtoad_usd..

    ELSE.
* US - 62758 - CBRAND - Fim
      "CS2021000657  / Anderson Oenning
      IF wa_saida-xivbrl IS INITIAL.
        wa_saida-xivbrl = xivbrl.
        wa_saida-xivusd = xivusd.
      ELSE.
        IF xivbrl IS NOT INITIAL AND xivusd IS NOT INITIAL.
          xivusd_ = xivbrl / xivusd.
          wa_saida-xivusd =  ( wa_saida-xivbrl / xivusd_ ).
        ENDIF.
      ENDIF.
      "CS2021000657  / Anderson Oenning

      wa_saida-xtoad_brl  = xtoad_brl.
      wa_saida-xtoad_usd  = xtoad_usd.
    ENDIF.

    wa_saida-disposto_brl = ( wa_saida-xtbrl_kob1  +  wa_saida-xtbrl_kob2  ).
    wa_saida-disposto_usd = ( wa_saida-xtusd_kob1  +  wa_saida-xtusd_kob2  ).

    wa_saida-disp_brl     = ( xivbrl -  wa_saida-disposto_brl ).
    wa_saida-disp_usd     = ( xivusd -  wa_saida-disposto_usd ).

    IF ( xivbrl NE 0 ).
      wa_saida-porc_brl   = ( wa_saida-disposto_brl / xivbrl ) * 100.
    ENDIF.

    IF ( xivusd NE 0 ).
      wa_saida-porc_usd   = ( wa_saida-disposto_usd / xivusd ) * 100.
    ENDIF.

    CONCATENATE wa_saida-bukrs wa_saida-gsber wa_saida-posnr  INTO  vl_obj_key.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZIMP60'
        objkey             = vl_obj_key
        client             = sy-mandt
      TABLES
        gos_connections    = anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    DESCRIBE TABLE anexos LINES vl_lines.

    IF vl_lines NE 0.
      wa_saida-anexo = '@FM@'.
    ELSE.
      wa_saida-anexo = '@02@'.
    ENDIF.

    PERFORM f_config_color_line CHANGING wa_saida.

**===========================================================================================
    "Imobilizado.
    CLEAR: vg_anln1.
    vg_anln1 = |{ wa_saida-objto ALPHA = IN }|.

    LOOP AT it_ekkn INTO wa_ekkn WHERE anln1 EQ vg_anln1.

      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln  = wa_ekkn-ebeln
      AND ebelp  = wa_ekkn-ebelp.

        CLEAR: wa_doc_adto.

        wa_doc_adto-bukrs = wa_zim01_sol_ap_inv-bukrs.
        wa_doc_adto-ebeln = wa_ekbe-ebeln.
        wa_doc_adto-ebelp = wa_ekbe-ebelp.
        wa_doc_adto-vgabe = wa_ekbe-vgabe.


        CASE wa_doc_adto-vgabe.
          WHEN '4' OR 'C'.

            IF wa_doc_adto-vgabe EQ '4'.
              wa_doc_adto-ds_operacao = 'Adiantamento'.
            ELSE.
              wa_doc_adto-ds_operacao = 'Compensação Adiantamento'.
            ENDIF.

            "Buscando valor.
            LOOP AT it_bsik INTO wa_bsik WHERE bukrs  = wa_zim01_sol_ap_inv-bukrs  AND
            belnr  = wa_ekbe-belnr AND
            gjahr  = wa_ekbe-gjahr AND
            buzei  = wa_ekbe-buzei_fi.

              wa_doc_adto-belnr = wa_bsik-belnr.
              wa_doc_adto-gjahr = wa_bsik-gjahr.
              wa_doc_adto-buzei = wa_bsik-buzei.
              wa_doc_adto-budat = wa_bsik-budat.

              IF wa_bsik-shkzg = 'H'.
                xtoad_brl =  xtoad_brl + ( wa_bsik-dmbtr  * - 1 ).
                xtoad_usd =  xtoad_usd + ( wa_bsik-dmbe2  * - 1 ).

                wa_doc_adto-dmbtr = ( wa_bsik-dmbtr  * - 1 ).
                wa_doc_adto-dmbe2 = ( wa_bsik-dmbe2  * - 1 ).

              ELSE.
                xtoad_brl =  xtoad_brl + wa_bsik-dmbtr.
                xtoad_usd =  xtoad_usd + wa_bsik-dmbe2.

                wa_doc_adto-dmbtr = wa_bsik-dmbtr.
                wa_doc_adto-dmbe2 = wa_bsik-dmbe2.
              ENDIF.

              APPEND wa_doc_adto TO wa_saida-docs_adto.

              xdtinicial =  wa_bsik-budat.
              xdtfinal   =  wa_bsik-budat.

              IF xdt_ini IS INITIAL.
                xdt_ini = xdtinicial.
              ELSE.
                IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
                  xdt_ini = xdtinicial.
                ENDIF.
              ENDIF.

              ADD wa_doc_adto-dmbtr TO  wa_saida-xtoad_brl.
              ADD wa_doc_adto-dmbe2 TO  wa_saida-xtoad_usd.
              CLEAR: wa_doc_adto, wa_bsik.
            ENDLOOP.

            LOOP AT it_bsak INTO wa_bsak WHERE bukrs  = wa_zim01_sol_ap_inv-bukrs  AND
            belnr  = wa_ekbe-belnr AND
            gjahr  = wa_ekbe-gjahr AND
            buzei  = wa_ekbe-buzei_fi.

              wa_doc_adto-belnr = wa_bsak-belnr.
              wa_doc_adto-gjahr = wa_bsak-gjahr.
              wa_doc_adto-buzei = wa_bsak-buzei.
              wa_doc_adto-budat = wa_bsak-budat.

              IF wa_bsak-shkzg = 'H'.
                xtoad_brl =  xtoad_brl + ( wa_bsak-dmbtr  * - 1 ).
                xtoad_usd =  xtoad_usd + ( wa_bsak-dmbe2  * - 1 ).

                wa_doc_adto-dmbtr = ( wa_bsak-dmbtr  * - 1 ).
                wa_doc_adto-dmbe2 = ( wa_bsak-dmbe2  * - 1 ).

              ELSE.
                xtoad_brl =  xtoad_brl + wa_bsak-dmbtr.
                xtoad_usd =  xtoad_usd + wa_bsak-dmbe2.

                wa_doc_adto-dmbtr = wa_bsak-dmbtr.
                wa_doc_adto-dmbe2 = wa_bsak-dmbe2.
              ENDIF.

              APPEND wa_doc_adto TO wa_saida-docs_adto.

              xdtinicial =  wa_bsak-budat.
              xdtfinal   =  wa_bsak-budat.

              IF xdt_ini IS INITIAL.
                xdt_ini = xdtinicial.
              ELSE.
                IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
                  xdt_ini = xdtinicial.
                ENDIF.
              ENDIF.

              ADD wa_doc_adto-dmbtr TO  wa_saida-xtoad_brl.
              ADD wa_doc_adto-dmbe2 TO  wa_saida-xtoad_usd.
              CLEAR: wa_doc_adto, wa_bsak.
            ENDLOOP.

          WHEN '2'.

            READ TABLE it_bkpf_aux INTO DATA(ws_bkpf_aux) WITH KEY belnr = wa_ekbe-belnr gjahr = wa_ekbe-gjahr buzei_fi = wa_ekbe-buzei_fi.
            IF sy-subrc EQ 0.
              READ TABLE it_bkpf INTO DATA(ws_bkpf) WITH KEY awkey = ws_bkpf_aux-awkey.
              IF sy-subrc EQ 0.

                LOOP AT it_bsik INTO wa_bsik WHERE belnr  = ws_bkpf-belnr AND
                gjahr  = ws_bkpf-gjahr.

                  xdtinicial  = wa_bsik-budat.
                  xdtfinal    = wa_bsik-budat.

                  CLEAR wa_doc_real.

                  wa_doc_real-wogbtr        = wa_bsik-dmbtr.
                  wa_doc_real-wkgbtr        = wa_bsik-dmbe2.

                  wa_doc_real-budat         = wa_bsik-budat.
                  wa_doc_real-ebeln         = wa_ekbe-ebeln.
                  wa_doc_real-ebelp         = wa_ekbe-ebelp.
                  wa_doc_real-bukrs         = wa_bsik-bukrs.
                  wa_doc_real-belnr_fi      = wa_bsik-belnr.
                  wa_doc_real-gjahr         = wa_bsik-gjahr.
                  wa_doc_real-buzei         = wa_bsik-buzei.
                  wa_doc_real-refbn         = ws_bkpf-xblnr.
                  wa_doc_real-refgj         = wa_bsik-gjahr.
*              wa_doc_real-document_key  = wa_ekbe-document_key.

                  APPEND wa_doc_real TO wa_saida-docs_real.

                  IF xdt_ini IS INITIAL.
                    xdt_ini = xdtinicial.
                  ELSE.
                    IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
                      xdt_ini = xdtinicial.
                    ENDIF.
                  ENDIF.

                  IF xdt_fim IS INITIAL.
                    xdt_fim = xdtfinal.
                  ELSE.
                    IF ( xdtfinal > xdt_fim ) AND ( xdtfinal IS NOT INITIAL ).
                      xdt_fim = xdtfinal.
                    ENDIF.
                  ENDIF.

                  xtbrl_kob1 =  xtbrl_kob1 + wa_doc_real-wogbtr.
                  xtusd_kob1 =  xtusd_kob1 + wa_doc_real-wkgbtr.

                  ADD wa_doc_real-wogbtr TO  wa_saida-xtbrl_kob1.
                  ADD wa_doc_real-wkgbtr TO  wa_saida-xtusd_kob1.
                  CLEAR: wa_doc_real, wa_bsik.
                ENDLOOP.

                LOOP AT it_bsak INTO wa_bsak WHERE belnr  = ws_bkpf-belnr AND
                gjahr  = ws_bkpf-gjahr.

                  wa_doc_real-wogbtr        = wa_bsak-dmbtr.
                  wa_doc_real-wkgbtr        = wa_bsak-dmbe2.
                  wa_doc_real-budat         = wa_bsak-budat.
                  wa_doc_real-ebeln         = wa_ekbe-ebeln.
                  wa_doc_real-ebelp         = wa_ekbe-ebelp.
                  wa_doc_real-bukrs         = wa_bsak-bukrs.
                  wa_doc_real-belnr_fi      = wa_bsak-belnr.
                  wa_doc_real-gjahr         = wa_bsak-gjahr.
                  wa_doc_real-buzei         = wa_bsak-buzei.
                  wa_doc_real-refbn         = ws_bkpf-xblnr.
                  wa_doc_real-refgj         = wa_bsak-gjahr.


*              wa_doc_real-document_key  = wa_ekbe-document_key.

                  APPEND wa_doc_real TO wa_saida-docs_real.

                  IF xdt_ini IS INITIAL.
                    xdt_ini = xdtinicial.
                  ELSE.
                    IF ( xdtinicial < xdt_ini ) AND ( xdtinicial IS NOT INITIAL ).
                      xdt_ini = xdtinicial.
                    ENDIF.
                  ENDIF.

                  IF xdt_fim IS INITIAL.
                    xdt_fim = xdtfinal.
                  ELSE.
                    IF ( xdtfinal > xdt_fim ) AND ( xdtfinal IS NOT INITIAL ).
                      xdt_fim = xdtfinal.
                    ENDIF.
                  ENDIF.

                  xtbrl_kob1 =  xtbrl_kob1 + wa_doc_real-wogbtr.
                  xtusd_kob1 =  xtusd_kob1 + wa_doc_real-wkgbtr.

                  ADD wa_doc_real-wogbtr TO  wa_saida-xtbrl_kob1.
                  ADD wa_doc_real-wkgbtr TO  wa_saida-xtusd_kob1.
                  CLEAR: wa_doc_real, wa_bsak.
                ENDLOOP.
              ENDIF.
            ENDIF.
        ENDCASE.
      ENDLOOP. "LOOP AT IT_EKBE
      CLEAR: wa_saida-disposto_brl, wa_saida-disp_brl, wa_saida-porc_brl, wa_saida-porc_usd.
    ENDLOOP.


* US - 62758 - Inicio - CBRAND
*    IF wa_saida-xtoad_brl IS INITIAL.
*      "Selecionar o pedido referente o imobilizado.
*      "Imobilizado.
*      CLEAR: vg_anln1.
*      vg_anln1 = |{ wa_saida-objto ALPHA = IN }|.
*
*      SELECT * FROM ekkn
*        INTO TABLE @DATA(lit_ekkn)
*      WHERE anln1 EQ @vg_anln1.
*
*      IF lit_ekkn IS NOT INITIAL.
*        "Selecionar tabela ekbe .
*        SELECT * FROM ekbe
*          INTO TABLE @DATA(lit_ekbe)
*        FOR ALL ENTRIES IN @lit_ekkn
*          WHERE ebeln EQ @lit_ekkn-ebeln
*            AND vgabe EQ '4'.
*
*        IF lit_ekbe IS NOT INITIAL.
*          LOOP AT lit_ekbe INTO DATA(lwa_ebke).
*            lva_dmbtr = lva_dmbtr + lwa_ebke-dmbtr.
*          ENDLOOP.
*          wa_saida-xtoad_brl = lva_dmbtr.
*          CLEAR: lva_dmbtr.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
* US - 62758 - Fim - CBRAND

    IF wa_saida-disposto_brl IS INITIAL.
      wa_saida-disposto_brl = ( wa_saida-xtbrl_kob1  +  wa_saida-xtbrl_kob2  ).
      wa_saida-disposto_usd = ( wa_saida-xtusd_kob1  +  wa_saida-xtusd_kob2  ).
    ENDIF.

*      IF wa_saida-disp_brl IS INITIAL.
    wa_saida-disp_brl     = ( wa_saida-xivbrl -  wa_saida-disposto_brl ).
    wa_saida-disp_usd     = ( wa_saida-xivusd -  wa_saida-disposto_usd ).
*      ENDIF.

*      IF wa_saida-porc_brl IS INITIAL.
    IF ( wa_saida-disposto_brl NE 0 ) AND ( wa_saida-xivbrl IS NOT INITIAL ).
      wa_saida-porc_brl   = ( wa_saida-disposto_brl / wa_saida-xivbrl ) * 100.
    ENDIF.
*      ENDIF.

*      IF wa_saida-porc_usd IS INITIAL.
    IF ( wa_saida-disposto_usd NE 0 ) AND ( wa_saida-xivusd IS NOT INITIAL ).
      wa_saida-porc_usd   = ( wa_saida-disposto_usd / wa_saida-xivusd  ) * 100.
    ENDIF.
*      ENDIF.


    IF wa_saida-dias_s_mov IS INITIAL AND wa_saida-dt_inicio IS NOT INITIAL.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from = wa_saida-dt_inicio
          i_date_to   = sy-datum
        IMPORTING
          e_days      = wa_saida-dias_s_mov.
    ENDIF.

    ls_aufnr = wa_saida-objto.
    ls_aufnr = |{ ls_aufnr ALPHA = IN }|.

    SELECT SINGLE * FROM zim13 INTO @DATA(w_zim13)
          WHERE bukrs EQ @wa_zim01_sol_ap_inv-bukrs
          AND   ano   EQ @wa_zim01_sol_ap_inv-ano
          AND   posnr EQ @wa_zim01_sol_ap_inv-posnr
          AND   aufnr EQ @ls_aufnr.

    IF sy-subrc EQ 0.
      wa_saida-status = w_zim13-status.

* US - 62758 - Inicio - CBRAND
*      IF w_zim13-oberv IS INITIAL.
*        wa_saida-observacao    = '@1F@'.
*      ELSE.
*        wa_saida-observacao    = '@1E@'.
*        wa_saida-texto         = w_zim13-oberv.
*      ENDIF.
*      wa_saida-dt_fim = w_zim13-dt_fim.
*    ELSE.
*      wa_saida-observacao    = '@1F@'.
*   ENDIF.

* US - 62758 - Inicio - CBRAND
      IF w_zim13-oberv IS NOT INITIAL.
        wa_saida-texto  = w_zim13-oberv.
      ENDIF.
      wa_saida-dt_fim = w_zim13-dt_fim.
    ENDIF.


*-CS2022001163-24.03.2023-#103539-JT-inicio
    "TIPOOBOJETO
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida-objto
      IMPORTING
        output = l_objeto.

    IF l_objeto = wa_saida-objnr+2. "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA

      SELECT CASE a~inact WHEN 'X' THEN 'Liberado' ELSE 'Bloqueado' END AS status
      FROM jest AS a
      INTO @wa_saida-ordstat
      WHERE a~objnr = @wa_saida-objnr
      AND a~stat = 'I0043'.
      ENDSELECT.

      IF wa_saida-objnr+0(2) = 'OR' AND wa_saida-ordstat = abap_false.
        wa_saida-ordstat = 'Liberado'.
      ENDIF.


    ENDIF.

    "IF wa_saida-objnr+0(2) = 'OR'."115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
    IF  l_objeto IS NOT INITIAL."115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
      SELECT SINGLE a~anlkl
      FROM anla AS a WHERE a~anln1 = @l_objeto
            INTO @wa_saida-classimob.
    ENDIF.

    IF    l_objeto = wa_saida-objnr+2 AND wa_saida-objnr+0(2) = 'OR'."115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
      SELECT SINGLE b~anlkl
      FROM cobrb AS a
      INNER JOIN anla AS b ON b~anln1 = a~anln1
      WHERE a~objnr = @wa_saida-objnr
                  INTO @wa_saida-classimob.
    ENDIF.





    IF l_objeto IS NOT INITIAL.
      IF      l_objeto  IN r_obj_faixa1[].
        wa_saida-objtotipo  = 'ORDEM ESTATÍSTICA'.
      ELSEIF  l_objeto IN r_obj_faixa2[]
        AND   wa_saida-classimob <> '00070000'."115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
        wa_saida-objtotipo  = 'ORDEM DE INVESTIMENTO'.
      ELSEIF  l_objeto+0(3) = 'OBR'
        OR    l_objeto+0(3) = 'EQP'. ""115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
        wa_saida-objtotipo = 'OBRAS EM ANDAMENTO'.
      ELSEIF  wa_saida-classimob = '00070000'.
        IF wa_saida-objnr+0(2) = 'OR'.
          wa_saida-objtotipo  = 'ORDEM DE REFLORESTAMENTO'.
        ELSE.
          wa_saida-objtotipo = 'IMOBILIZADO REFLORESTAMENTO'.
        ENDIF.
      ELSE.
        wa_saida-objtotipo  = 'ATIVO IMOBILIZADO'.
      ENDIF.
    ENDIF.

*-CS2022001163-24.03.2023-#103539-JT-fim

    APPEND wa_saida TO it_saida_aux.

    CLEAR:  wa_zim01_sol_ap_inv, wa_saida,  wa_tka02,   wa_cskt,  wa_imakz,  wa_coas,
    wa_imaka, wa_anla, wa_anep,  wa_anep_usd,  wa_ekkn, wa_ekbe,  wa_bsik,
    wa_bsak,  w_zim13, anexos, wa_ordem, ws_bkpf, w_bpja, ws_bkpf_aux, t_bpja, w_ordem.

    CLEAR:  xtbrl_kob1,   xtusd_kob1,   xtbrl_kob2,   xtusd_kob2,  xtotbrl_imob,
    xtotusd_imob, xtoad_brl, xtoad_usd,  xivbrl,  xivusd, xdtiadto,vl_lines,
    xdtfadto,  xdtiimob, xdtfimob,  xdtikob1,  xdtfkob1, xdtinicial,  xdtfinal, vl_obj_key, xivusd_.

  ENDLOOP.



  REFRESH it_objnr.

  LOOP AT p_aufnr.
    w_objnr-sign   = p_aufnr-sign.
    w_objnr-option = p_aufnr-option.
    w_objnr-low    = |{ p_aufnr-low ALPHA  = OUT }|.
    IF p_aufnr-high IS NOT INITIAL.
      w_objnr-high   = |{ p_aufnr-high ALPHA  = OUT }|.
    ENDIF.
    APPEND w_objnr TO it_objnr.
    CLEAR w_objnr.
  ENDLOOP.

  IF p_aufnr IS NOT INITIAL.
    LOOP AT it_saida_aux INTO DATA(wsaida)
          WHERE  objto IN it_objnr.
      APPEND wsaida TO it_saida.
      CLEAR wsaida.
    ENDLOOP.
  ELSE.
    MOVE it_saida_aux TO it_saida.
  ENDIF.

*-CS2022001163-24.03.2023-#103539-JT-inicio *** JAIME
  DATA: tl_docs_real LIKE TABLE OF t_docs_real,
        l_totbrl     TYPE ty_saida-xtbrl_kob2,
        l_totusd     TYPE ty_saida-xtbrl_kob2,
        l_data       TYPE datum,
        l_taxa       TYPE ukurs_curr.

  it_saida_ativo[] = it_saida[].
  DELETE it_saida_ativo WHERE objtotipo <> 'ATIVO IMOBILIZADO'.

  CHECK it_saida_ativo[] IS NOT INITIAL.

  LOOP AT it_saida_ativo INTO wa_saida_ativo.
    l_tabix = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida_ativo-objto
      IMPORTING
        output = wa_saida_ativo-objto.

    MODIFY it_saida_ativo FROM wa_saida_ativo INDEX l_tabix.
  ENDLOOP.

  SELECT bukrs  anln1  anln2  lnran afabe  bwasl  bzdat   anbtr
    FROM anep
    INTO TABLE it_anep_imob_ger
     FOR ALL ENTRIES IN it_saida_ativo
   WHERE bukrs EQ it_saida_ativo-bukrs
     AND anln1 EQ it_saida_ativo-objto
     AND anln2 EQ it_saida_ativo-anln2
     AND afabe EQ '01'
     AND bzdat IN p_data.

  SELECT bukrs  anln1  anln2  lnran afabe  bwasl  bzdat   anbtr
    FROM anep
    INTO TABLE it_anep_usd_imob_ger
     FOR ALL ENTRIES IN it_saida_ativo
   WHERE bukrs EQ it_saida_ativo-bukrs
     AND anln1 EQ it_saida_ativo-objto
     AND anln2 EQ it_saida_ativo-anln2
     AND afabe EQ '41'
     AND bzdat IN p_data.

  SELECT bukrs anln1 anln2 txt50 lkauf aibn1 deakt
    FROM anla
    INTO TABLE it_anla_imob
     FOR ALL ENTRIES IN it_saida_ativo
   WHERE bukrs EQ it_saida_ativo-bukrs
     AND anln1 EQ it_saida_ativo-objto
     AND anln2 EQ it_saida_ativo-anln2.

  SELECT *
    INTO TABLE it_ekkn_imob
    FROM ekkn
     FOR ALL ENTRIES IN it_saida_ativo
   WHERE anln1 = it_saida_ativo-objto
     AND anln2 = it_saida_ativo-anln2.

*  IF it_ekkn_imob[] IS NOT INITIAL.
*    SELECT *
*      INTO TABLE it_ekpo_imob
*      FROM ekpo
*       FOR ALL ENTRIES IN it_ekkn_imob
*     WHERE ebeln = it_ekkn_imob-ebeln
*       AND ebelp = it_ekkn_imob-ebelp.
*  ENDIF.
*
*  IF it_ekpo_imob[] IS NOT INITIAL.
*    SELECT *
*      INTO TABLE it_ekbe_imob
*      FROM ekbe
*       FOR ALL ENTRIES IN it_ekpo_imob
*     WHERE ebeln  = it_ekpo_imob-ebeln
*       AND ebelp  = it_ekpo_imob-ebelp
*       AND vgabe  = '2'
*       AND budat IN p_data.
*  ENDIF.

  LOOP AT it_saida  INTO wa_saida.

    l_tabix = sy-tabix.

    CLEAR: wa_ekkn_imob, wa_ekpo_imob.

    CHECK wa_saida-objtotipo = 'ATIVO IMOBILIZADO'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_saida-objto
      IMPORTING
        output = wa_saida-objto2.

    FREE: tl_docs_real.
    LOOP AT it_ekkn_imob INTO wa_ekkn_imob WHERE anln1 = wa_saida-objto2
                                             AND anln2 = wa_saida-anln2.
      wa_doc_real-ebeln     = wa_ekkn_imob-ebeln.
      wa_doc_real-ebelp     = wa_ekkn_imob-ebelp.
      APPEND wa_doc_real   TO tl_docs_real.
    ENDLOOP.

*   READ TABLE it_ekkn_imob INTO wa_ekkn_imob WITH KEY anln1 = wa_saida-objto.
*   READ TABLE it_ekpo_imob INTO wa_ekpo_imob WITH KEY ebeln = wa_ekkn_imob-ebeln
*                                                      ebelp = wa_ekkn_imob-ebelp.
*
*   CLEAR l_dmbtr.
*   LOOP AT it_ekbe_imob INTO wa_ekbe_imob WHERE ebeln = wa_ekkn_imob-ebeln
*                                            AND ebelp = wa_ekkn_imob-ebelp.
*     IF     wa_ekbe_imob-shkzg = 'S'.
*       l_dmbtr = l_dmbtr + wa_ekbe_imob-dmbtr.
*     ELSEIF wa_ekbe_imob-shkzg = 'H'.
*       l_dmbtr = l_dmbtr - wa_ekbe_imob-dmbtr.
*     ENDIF.
*   ENDLOOP.
*
*   wa_saida-disposto_brl = wa_ekpo_imob-netwr.
*   wa_saida-disp_brl     = l_dmbtr.

    CLEAR: wa_saida-xtbrl_kob1, wa_saida-xtusd_kob1, l_totitens,
           wa_saida-xtbrl_kob2, wa_saida-xtusd_kob2, l_totbrl, l_totusd.

    IF tl_docs_real[] IS NOT INITIAL.
*     SELECT COUNT(*)
*       INTO l_totitens
*       FROM ekpo
*        FOR ALL ENTRIES IN tl_docs_real
*      WHERE ebeln = tl_docs_real-ebeln
*        AND loekz = abap_off.

      SELECT ekpo~ebeln ekpo~ebelp ekko~waers ekpo~netpr
        INTO TABLE it_ekpo_imob
        FROM ekpo
       INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
         FOR ALL ENTRIES IN tl_docs_real
       WHERE ekpo~ebeln = tl_docs_real-ebeln
         AND ekpo~ebelp = tl_docs_real-ebelp
         AND ekpo~knttp = 'A'
         AND ekpo~loekz = abap_off.

      SELECT ebeln ebelp zekkn vgabe gjahr belnr buzei
        INTO TABLE it_ekbe_imob
        FROM ekbe
         FOR ALL ENTRIES IN tl_docs_real
       WHERE ebeln = tl_docs_real-ebeln
         AND ebelp = tl_docs_real-ebelp.

      SELECT *
        INTO TABLE it_eket_imob
        FROM eket
         FOR ALL ENTRIES IN tl_docs_real
       WHERE ebeln = tl_docs_real-ebeln
         AND ebelp = tl_docs_real-ebelp.
    ENDIF.

    LOOP AT it_anep_imob_ger     INTO wa_anep_imob     WHERE bukrs = wa_saida-bukrs
                                                         AND anln1 = wa_saida-objto2
                                                         AND anln2 = wa_saida-anln2.
      wa_saida-xtbrl_kob1 = wa_saida-xtbrl_kob1 + wa_anep_imob-anbtr.
    ENDLOOP.

    LOOP AT it_anep_usd_imob_ger INTO wa_anep_usd_imob WHERE bukrs = wa_saida-bukrs
                                                         AND anln1 = wa_saida-objto2
                                                         AND anln2 = wa_saida-anln2.
      wa_saida-xtusd_kob1 = wa_saida-xtusd_kob1 + wa_anep_usd_imob-anbtr.
    ENDLOOP.

    LOOP AT tl_docs_real INTO wa_doc_real.
      CLEAR: wa_ekbe_imob, wa_eket_imob, wa_ekpo_imob.

      READ TABLE it_ekbe_imob INTO wa_ekbe_imob WITH KEY ebeln = wa_doc_real-ebeln
                                                         ebelp = wa_doc_real-ebelp.
      CHECK sy-subrc <> 0.

      READ TABLE it_eket_imob INTO wa_eket_imob WITH KEY ebeln = wa_doc_real-ebeln
                                                         ebelp = wa_doc_real-ebelp.
      CHECK sy-subrc = 0.

      READ TABLE it_ekpo_imob INTO wa_ekpo_imob WITH KEY ebeln = wa_doc_real-ebeln
                                                         ebelp = wa_doc_real-ebelp.
      CHECK sy-subrc = 0.

      PERFORM f_busca_taxa USING wa_eket_imob-eindt
                        CHANGING l_taxa.

      IF wa_ekpo_imob-waers = 'BRL'.
        l_totbrl = l_totbrl + wa_ekpo_imob-netpr.
        l_totusd = l_totusd + wa_ekpo_imob-netpr / l_taxa.
      ELSE.
        l_totusd = l_totusd + wa_ekpo_imob-netpr.
        l_totbrl = l_totbrl + wa_ekpo_imob-netpr * l_taxa.
      ENDIF.
    ENDLOOP.

    CLEAR l_totitens.


    READ TABLE t_ordem INTO wa_ordem WITH KEY objnr = wa_saida-objto2
                                              anln2 = wa_saida-anln2.
    IF sy-subrc = 0.
      READ TABLE it_anla_imob INTO wa_anla_imob WITH KEY bukrs = wa_saida-bukrs
                                                         anln1 = wa_saida-objto2
                                                         anln2 = wa_saida-anln2.


      IF sy-subrc = 0 AND wa_anla_imob-lkauf <> 0.
        l_totitens = 1.
      ELSE.
        LOOP AT t_ordem INTO wa_ordem WHERE posnr = wa_ordem-posnr.
          l_totitens = l_totitens + 1.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF l_totitens <> 0.
      wa_saida-xivbrl     = wa_saida-xivbrl / l_totitens.
      wa_saida-xivusd     = wa_saida-xivusd / l_totitens.
    ENDIF.

    wa_saida-xtbrl_kob2   = l_totbrl.
    wa_saida-xtusd_kob2   = l_totusd.
    wa_saida-disposto_brl = wa_saida-xtbrl_kob1 + wa_saida-xtbrl_kob2.
    wa_saida-disposto_usd = wa_saida-xtusd_kob1 + wa_saida-xtusd_kob2.
    wa_saida-disp_brl     = wa_saida-xivbrl     - wa_saida-disposto_brl.
    wa_saida-disp_usd     = wa_saida-xivusd     - wa_saida-disposto_usd.

*   wa_saida-xtbrl_kob1_ori = wa_saida-xtbrl_kob1.
*   wa_saida-xtusd_kob1_ori = wa_saida-xtusd_kob1.


    MODIFY it_saida FROM wa_saida INDEX l_tabix.
  ENDLOOP.
*-CS2022001163-24.03.2023-#103539-JT-fim

ENDFORM.


FORM busca_dados_kob1.

  DATA: it_kob1_tmp TYPE TABLE OF ty_kob1.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-tabix
      text       = 'Extraindo dados em  KOB1...'.

  PERFORM f_prepare_run_time_info USING abap_false.


  SUBMIT rkaep000  WITH p_tcode EQ 'KOB1'
  WITH aufnr   IN it_ordem
  WITH p_kokrs EQ wa_tka02-kokrs
  WITH r_budat BETWEEN p_data-low AND p_data-high
  WITH p_usedb  = 'X'
  WITH p_usear  = ' '
  WITH p_maxsel  = 200000
  WITH p_disvar = '/CONFERENCIA'
  AND RETURN.

  PERFORM f_get_runtime_info.
  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR tg_entrada.
      MOVE-CORRESPONDING <w_data> TO tg_entrada.
      APPEND tg_entrada.
    ENDLOOP.
  ENDIF.

  DELETE tg_entrada WHERE owaer  NE 'BRL'.

  SELECT *
  FROM setleaf INTO TABLE @DATA(tg_setleaf_exclude_kstat)
        WHERE setname EQ 'ZIM14_KOB1_EXCLUDE_KSTAR'.

  LOOP AT tg_setleaf_exclude_kstat INTO DATA(wl_setleaf).
    DELETE tg_entrada WHERE kstar  EQ wl_setleaf-valfrom.
  ENDLOOP.

  LOOP AT tg_entrada INTO DATA(entrada).

    wa_kob1-aufnr    = entrada-aufnr.
    wa_kob1-budat    = entrada-budat.
    wa_kob1-bldat    = entrada-bldat.
    wa_kob1-wkgbtr   = entrada-wkgbtr.
    wa_kob1-wogbtr   = entrada-wogbtr.
    wa_kob1-ebeln    = entrada-ebeln.
    wa_kob1-ebelp    = entrada-ebelp.
    wa_kob1-bukrs    = entrada-bukrs.
    wa_kob1-gjahr    = entrada-gjahr.
    wa_kob1-buzei    = entrada-buzei.
    wa_kob1-refbn    = entrada-refbn.
    wa_kob1-refgj    = entrada-refgj.
    wa_kob1-awkey    = wa_kob1-refbn && wa_kob1-refgj.

    wa_kob1-document_key-kokrs     = entrada-kokrs.
    wa_kob1-document_key-vrgng     = entrada-vrgng.
    wa_kob1-document_key-orgvg     = entrada-orgvg.
    wa_kob1-document_key-varnr     = entrada-varnr.
    wa_kob1-document_key-belnr     = entrada-belnr.
    wa_kob1-document_key-buzei     = entrada-buzei.
    wa_kob1-document_key-refbt     = entrada-refbt.
    wa_kob1-document_key-refbn     = entrada-refbn.
    wa_kob1-document_key-refbk     = entrada-refbk.
    wa_kob1-document_key-refgj     = entrada-refgj.
    wa_kob1-document_key-awtyp     = entrada-awtyp.
    wa_kob1-document_key-aworg     = entrada-aworg.
    wa_kob1-document_key-logsystem = entrada-logsystem.


    APPEND wa_kob1 TO it_kob1.
    CLEAR: wa_kob1, entrada.
  ENDLOOP.

  CLEAR: tg_bkpf_tmp[].

  it_kob1_tmp[] = it_kob1[].
  DELETE it_kob1_tmp WHERE awkey IS INITIAL.

  IF it_kob1_tmp[] IS NOT INITIAL.
    SELECT *
    FROM bkpf INTO TABLE tg_bkpf_tmp
    FOR ALL ENTRIES IN it_kob1_tmp
    WHERE bukrs EQ it_kob1_tmp-bukrs
    AND awkey EQ it_kob1_tmp-awkey.

    IF tg_bkpf_tmp[] IS NOT INITIAL.
      LOOP AT it_kob1 ASSIGNING FIELD-SYMBOL(<fs_kob1>).

        READ TABLE tg_bkpf_tmp WITH KEY bukrs = <fs_kob1>-bukrs
        awkey = <fs_kob1>-awkey.
        IF sy-subrc EQ 0.
          <fs_kob1>-belnr_fi = tg_bkpf_tmp-belnr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.


FORM busca_dados_kob2.

  DATA: it_kob1_tmp TYPE TABLE OF ty_kob1.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-tabix
      text       = 'Extraindo dados em  KOB2...'.

  PERFORM f_prepare_run_time_info USING abap_false.

  SUBMIT rkaep000  WITH p_tcode EQ 'KOB2'
  WITH aufnr   IN it_ordem
  WITH p_kokrs EQ wa_tka02-kokrs
  WITH r_budat BETWEEN p_data-low AND '99991231'"p_data-high - Ajuste para selecionar.
  WITH p_usedb = 'X'
  WITH p_usear = ' '
  WITH p_maxsel  = 200000
  WITH p_disvar = '/CONFERENCIA'
  AND RETURN .

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR tg_entrada_02.
      MOVE-CORRESPONDING <w_data> TO tg_entrada_02.
      APPEND tg_entrada_02.
    ENDLOOP.
  ENDIF.

  DELETE tg_entrada_02 WHERE owaer  NE 'BRL'.
  DELETE tg_entrada_02 WHERE wogbtr EQ 0.

  LOOP AT tg_entrada_02 INTO DATA(entrada02).
    wa_kob2-aufnr    = entrada02-aufnr.
    wa_kob2-budat    = entrada02-budat.
    wa_kob2-bldat    = entrada02-bldat.
    wa_kob2-wkgbtr   = entrada02-wkgbtr.
    wa_kob2-wogbtr   = entrada02-wogbtr.
    wa_kob2-ebeln    = entrada02-ebeln.
    wa_kob2-ebelp    = entrada02-ebelp.
    wa_kob2-bukrs    = entrada02-bukrs.
    wa_kob2-gjahr    = entrada02-gjahr.
    wa_kob2-buzei    = entrada02-buzei.
    wa_kob2-refbn    = entrada02-refbn.
    wa_kob2-refgj    = entrada02-refgj.
    wa_kob2-awkey    = wa_kob2-refbn && wa_kob2-refgj.

    wa_kob2-document_key-refbt = entrada02-refbt.
    wa_kob2-document_key-refbn = entrada02-refbn.
    wa_kob2-document_key-rfpos = entrada02-rfpos.
    wa_kob2-document_key-gjahr = entrada02-gjahr.

    APPEND wa_kob2 TO it_kob2.
    CLEAR: wa_kob2, entrada02.
  ENDLOOP.

ENDFORM.

FORM f_prepare_run_time_info USING p_display TYPE c.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line, l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = p_display
    metadata = abap_false
  data     = abap_true ).
ENDFORM.

FORM f_get_runtime_info.
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr  = l_data_descr
        r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data>
        t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.
ENDFORM.

FORM alv.

  PERFORM preenche_cat USING :
   '01' 'BUKRS'                         'Empresa'               '07'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '02' 'GSBER'                         'Filial'                '07'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '03' 'POSNR'                         'Sol.Invest'            '10'     ''    'X'     ''    ''   ''    ''            ''  ''  '',
   '04' 'KOSTL_OUT'                     'Centro Custo'          '10'     ''    'X'     ''    ''   ''    ''            ''  ''  '',
   '05' 'LTEXT'                         'Descrição C.Custo'     '25'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '06' 'DESCR_ITEM'                    'Desc.Investimento'     '50'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '07' 'DT_INICIO'                     'Data Inicio'           '10'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '08' 'DT_FIM'                        'Data Fim'              '10'     ''    ''     ''    ''   ''    vg_ac_save    ''  ''  '',
   '09' 'OBJTO'                         'Objeto'                '10'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '10' 'OBJTOTIPO'                     'Tipo de Objeto'        '40'     ''    ''     ''    ''   ''    ''            ''  ''  '',  "*-CS2022001163-24.03.2023-#103539-JT
   '11' 'DESC_OBJTO'                    'Descrição Objeto'      '25'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '12' 'XIVBRL'                        'Valor Orçamento BRL'   '14'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '13' 'XIVUSD'                        'Valor Orçamento USD'   '14'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '14' 'XTBRL_KOB1'                    'Realizado BRL'         '14'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '15' 'XTUSD_KOB1'                    'Realizado USD'         '14'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '16' 'XTBRL_KOB2'                    'Compromisso BRL'       '14'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '17' 'XTUSD_KOB2'                    'Compromisso USD'       '14'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '18' 'XTOAD_BRL'                     'Adiantamento BRL'      '16'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '19' 'XTOAD_USD'                     'Adiantamento USD'      '16'     ''    'X'    ''    ''   ''    ''            ''  ''  '',
   '20' 'DISPOSTO_BRL'                  'Total BRL'             '20'     ''    ''     ''    ''   ''    ''            ''  ''  '', "Disposto BRL "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
   '21' 'DISPOSTO_USD'                  'Total USD'             '14'     ''    ''     ''    ''   ''    ''            ''  ''  '', "Disposto USD "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
   '22' 'DISP_BRL'                      'Disponível BRL'        '14'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '23' 'DISP_USD'                      'Disponível USD'        '14'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '24' 'PORC_BRL'                      '% BRL'                 '05'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '25' 'PORC_USD'                      '% USD'                 '05'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '26' 'DIAS_S_MOV'                    'Dias Sem Mov.'         '13'     ''    ''     ''    ''   ''    ''            ''  ''  '',
   '27' 'STATUS'                        'Status'                '06'     ''    ''     ''    ''   ''    vg_ac_save    ''  ''  'X',
"  '28' 'OBSERVACAO'                    'Obs.'                  '02'     ''    ''     ''    ''   'X'    ''           ''  ''  '', US - 62758 - CSB
   '29' 'TEXTO'                         'Obs.'                  '45'     ''    ''     ''    ''   ''    'X'           ''  ''  '', "US - 62758 - CSB
   '30' 'ANEXO'                         'Anexo'                 '10'     ''    ''     ''    ''   'X'    ''           ''  ''  '',
   '31' 'AIBN1'                         'Imobilizado Origem'    '18'     ''    ''     ''    ''   ''    ''            ''  ''  '', "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
   '32' 'ORDSTAT'                       'Ordem Status'          '18'     ''    ''     ''    ''   ''    ''            ''  ''  '', "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
   '33' 'DEAKT'                         'Desativado em'         '10'     ''    ''     ''    ''   ''    ''            ''  ''  '', "115204 CS2023000426 Transacao ZIM14 - AJUSTES 2023 PSA
   '34' 'SOLICITACAO_INVEST'            'ID SYSPHERA'         '10'     ''    ''     ''    ''   ''    ''            ''  ''  ''. "US #171475 - MMSILVA - 25.03.2025



  CALL SCREEN 0100.

ENDFORM.

FORM preenche_cat USING VALUE(p_pos)
                        VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_icon)
                        VALUE(p_edit)
                        VALUE(p_table)
                        VALUE(p_field)
                        VALUE(p_f4).

  wa_fieldcatalog-col_pos     = p_pos.
  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.

  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.
  wa_fieldcatalog-icon        = p_icon.
  wa_fieldcatalog-edit        = p_edit.
  wa_fieldcatalog-ref_table   = p_table.
  wa_fieldcatalog-ref_field   = p_field.
  wa_fieldcatalog-f4availabl  = p_f4.

* US - 62758 - Inicio - CBRAND
*  IF p_campo EQ 'OBSERVACAO' OR  p_campo EQ 'ANEXO'.
*    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
*  ELSE.
*    wa_fieldcatalog-style = ''.
*  ENDIF.

  IF   p_campo EQ 'ANEXO'.
    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
  ELSE.
    wa_fieldcatalog-style = ''.
  ENDIF.
* US - 62758 - Fim - CBRAND


  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.

FORM preenche_cat_imob USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot).

  CLEAR: wa_estrutura.

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
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat_imob.

ENDFORM.

FORM preenche_cat_comp USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot).

  CLEAR: wa_estrutura.

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
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat_comp.

ENDFORM.

FORM fill_gs_variant.
  gs_variant-report     = sy-repid.
  gs_variant-handle     = '0100'.
  gs_variant-log_group  = abap_false.
  gs_variant-username   = abap_false.
  gs_variant-text       = abap_false.
  gs_variant-dependvars = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: url(255)                TYPE c,
        data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i.

  DATA: it_ucomm TYPE TABLE OF sy-ucomm WITH HEADER LINE.

*  IF VG_AC_SAVE EQ ABAP_FALSE.
*    CLEAR: IT_UCOMM[].
*
*    IT_UCOMM = 'SALVAR'.
*    APPEND IT_UCOMM.
*
*    SET PF-STATUS 'STATUS' EXCLUDING IT_UCOMM.
*  ELSE.
*
*  ENDIF.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR  'TITULO'.

  IF g_custom_container IS INITIAL.

* create a container for the tree control
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 1.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 15.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 80.


    PERFORM fill_gs_variant.

    "GS_LAYOUT-SEL_MODE   = 'A'.
    "GS_LAYOUT-STYLEFNAME = 'CELLSTYLES'.
    gs_layout-ctab_fname = 'COLOR'.

    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        i_save          = 'A'
        is_variant      = variante
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_saida.

    CALL METHOD ctl_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH gt_f4.

    gt_f4-fieldname  = 'STATUS'.
    gt_f4-register   = 'X'.
    gt_f4-getbefore  = 'X'.
    gt_f4-chngeafter = 'X'.
    APPEND gt_f4.

    CALL METHOD ctl_alv->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER: obj_toolbar->on_toolbar          FOR ctl_alv,
    obj_toolbar->handle_user_command FOR ctl_alv.

    SET HANDLER: lcl_event_handler=>on_f4 FOR ctl_alv,
    lcl_event_handler=>catch_hotspot FOR ctl_alv,
    lcl_event_handler=>on_data_changed FOR ctl_alv,
    lcl_event_handler=>on_button_click FOR ctl_alv.

    CALL METHOD ctl_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        "SAP_ALIGN = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-002.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.
    "Empresa
    IF p_bukrs IS NOT INITIAL.
      SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
            WHERE bukrs IN @p_bukrs.

      LOOP AT p_bukrs.
        IF p_bukrs-option NE 'EQ' AND p_bukrs-option NE 'BT'.
          sdydo_text_element = 'Empresa: Multiplas Seleções'.
          EXIT.
        ELSEIF p_bukrs-option EQ 'BT'.

          CONCATENATE 'Empresa:' p_bukrs-low '-' wa_t001-butxt  INTO sdydo_text_element SEPARATED BY space.

          CONCATENATE sdydo_text_element p_bukrs-high INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Empresa: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Empresa:' p_bukrs-low  '-' wa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element, wa_t001.
    ENDIF.
    "-----------------
    " filial
    IF p_werks IS NOT  INITIAL.
      LOOP AT p_werks.
        IF p_werks-option NE 'EQ' AND p_werks-option NE 'BT'.
          sdydo_text_element = 'Filial: Multiplas Seleções'.
          EXIT.
        ELSEIF p_werks-option EQ 'BT'.
          CONCATENATE 'Filial:' p_werks-low  INTO sdydo_text_element SEPARATED BY space.
          CONCATENATE sdydo_text_element p_werks-high  INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Filial: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Filial:' p_werks-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.


    IF p_kostl IS NOT  INITIAL.
      LOOP AT p_kostl.
        IF p_kostl-option NE 'EQ' AND p_kostl-option NE 'BT'.
          sdydo_text_element = 'Centro de Custo: Multiplas Seleções'.
          EXIT.
        ELSEIF p_kostl-option EQ 'BT'.
          CONCATENATE 'Centro de Custo:' p_kostl-low   INTO sdydo_text_element SEPARATED BY space.
          CONCATENATE sdydo_text_element p_kostl-high  INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Centro de Custo: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Centro de Custo:' p_kostl-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.

    IF p_posnr IS NOT  INITIAL.
      LOOP AT p_posnr.
        IF p_posnr-option NE 'EQ' AND p_posnr-option NE 'BT'.
          sdydo_text_element = 'Nº Investimento: Multiplas Seleções'.
          EXIT.
        ELSEIF p_posnr-option EQ 'BT'.
          CONCATENATE 'Nº Investimento:' p_posnr-low   INTO sdydo_text_element SEPARATED BY space.
          CONCATENATE sdydo_text_element p_posnr-high  INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Nº Investimento: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Nº Investimento:' p_posnr-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.


    IF p_aufnr IS NOT  INITIAL.
      LOOP AT p_aufnr.
        IF p_aufnr-option NE 'EQ' AND p_aufnr-option NE 'BT'.
          sdydo_text_element = 'Nº Ordem: Multiplas Seleções'.
          EXIT.
        ELSEIF p_aufnr-option EQ 'BT'.
          CONCATENATE 'Nº Ordem:'        p_aufnr-low   INTO sdydo_text_element SEPARATED BY space.
          CONCATENATE sdydo_text_element p_aufnr-high  INTO sdydo_text_element SEPARATED BY space.
          EXIT.
        ELSE.
          vl_cont = vl_cont + 1.
          IF vl_cont GT 1.
            sdydo_text_element = 'Nº Ordem: Multiplas Seleções'.
          ELSE.
            CONCATENATE 'Nº Ordem:' p_aufnr-low  INTO sdydo_text_element SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.
      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element.
    ENDIF.


    "Exercio
    IF lines( r_ano[] ) EQ 1.
      READ TABLE r_ano INDEX 1.
      CONCATENATE 'Exercício: '  r_ano-low  INTO sdydo_text_element SEPARATED BY space.
    ELSE.
      SORT r_ano BY low.

      READ TABLE r_ano INDEX 1.
      DATA(v_ano_ini) = r_ano-low.

      DATA(index_fim) = lines( r_ano[] ).

      READ TABLE r_ano INDEX index_fim.
      DATA(v_ano_fim) = r_ano-low.

      CONCATENATE 'Exercício: '  v_ano_ini 'até' v_ano_fim  INTO sdydo_text_element SEPARATED BY space.

    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: vl_cont, sdydo_text_element.

    " Data lançamento
    IF p_data IS NOT INITIAL.
      CONCATENATE  p_data-low+6(2)  '.'  p_data-low+4(2)  '.' p_data-low(4)  INTO data_ini.
      CONCATENATE  p_data-high+6(2) '.'  p_data-high+4(2) '.' p_data-high(4) INTO data_fim.

      CONCATENATE 'Data Lançamento:  ' data_ini  INTO  sdydo_text_element SEPARATED BY space.
      IF data_fim <> '00.00.0000' .
        CONCATENATE  sdydo_text_element 'até'  data_fim  INTO sdydo_text_element SEPARATED BY space.
      ENDIF.

      APPEND sdydo_text_element TO p_text_table.
      CLEAR: vl_cont, sdydo_text_element,  data_ini, data_fim.
    ENDIF.
*
*    "------------------
    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

    CALL METHOD ctl_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ctl_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD ctl_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.

      PERFORM f_salvar_dados.


  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
*  SET PF-STATUS 'STATUS_02'.
*  SET TITLEBAR 'TITULO_02'.
*
*  IF VG_1PTELA IS NOT INITIAL.
*    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX VG_ROW.
*
*    IF WA_SAIDA-STATUS EQ '01'.
*      MOVE ABAP_TRUE TO ST01.
*
*    ELSEIF WA_SAIDA-STATUS EQ '02'.
*      MOVE ABAP_TRUE TO ST02.
*    ENDIF.
*
*    CLEAR: WA_SAIDA, VG_1PTELA.
*  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
*
*  CASE SY-UCOMM.
*    WHEN 'SALVAR'.
*      REFRESH IT_ZIM13.
*
*      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX VG_ROW.
*      IF SY-SUBRC = 0.
*        WA_ZIM13-BUKRS    = WA_SAIDA-BUKRS.
*        WA_ZIM13-ANO      = P_ANO-LOW.
*        WA_ZIM13-POSNR    = |{ WA_SAIDA-POSNR ALPHA = IN }|.
*        WA_ZIM13-AUFNR    = |{ WA_SAIDA-OBJTO ALPHA = IN }|.
*        IF ST01 IS NOT INITIAL.
*          WA_ZIM13-STATUS   =  '01'.
*          WA_SAIDA-STATUS   =  '01'.
*        ELSE.
*          WA_ZIM13-STATUS   =  '02'.
*          WA_SAIDA-STATUS   =  '02'.
*        ENDIF.
*
*        WA_ZIM13-USUARIO  = SY-UNAME.
*        WA_ZIM13-DT_MODF  = SY-DATUM.
*        WA_ZIM13-HR_MODF  = SY-UZEIT.
*        "        WA_ZIM13-OBERV    = WA_SAIDA-OBS.
*        APPEND WA_ZIM13 TO IT_ZIM13.
*      ENDIF.
*
*      MODIFY IT_SAIDA FROM WA_SAIDA INDEX VG_ROW.
*
**      IF IT_ZIM13[] IS NOT INITIAL.
**        MODIFY ZIM13 FROM TABLE IT_ZIM13.
**      ENDIF.
*
*      CLEAR: WA_SAIDA, VG_ROW, WA_ZIM13.
*
*      CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*      LEAVE TO SCREEN 0.
*
*    WHEN 'CANCEL'.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
ENDMODULE.


FORM f_resumo_documentos_comp USING p_saida  TYPE ty_saida.

  DATA : ls_variant TYPE disvariant.

  CHECK p_saida IS NOT INITIAL.

  CLEAR: tg_docs_comp_out[], estrutura[].

  tg_docs_comp_out[] = p_saida-docs_comp[].

  CHECK tg_docs_comp_out[] IS NOT INITIAL.

  SORT tg_docs_comp_out BY budat DESCENDING.

  PERFORM f_montar_estrutura USING:
        01  'KAEP_COEP_X'  'AUFNR'        'TG_DOCS_COMP_OUT'  'AUFNR'    'Objeto'          '10' '' ''   ''   '',
        02  'COBK'         'BUDAT'        'TG_DOCS_COMP_OUT'  'BUDAT'    'Dt.Lcto'         '10' '' ''   ''   '',
        03  'BSIS'         'DMBTR'        'TG_DOCS_COMP_OUT'  'WOGBTR'   'Valor R$'        '13' '' 'X'  ''   '',
        04  'BSIS'         'DMBE2'        'TG_DOCS_COMP_OUT'  'WKGBTR'   'Valor U$'        '13' '' 'X'  ''   '',
        "05  'EKPO'         'EBELN'        'TG_DOCS_COMP_OUT'  'EBELN'    'Pedido'          '10' '' ''   ''   'X',
        "06  'EKPO'         'EBELP'        'TG_DOCS_COMP_OUT'  'EBELP'    'It.Ped.'         '06' '' ''   ''   '',
        07  'BSIS'         'BUKRS'        'TG_DOCS_COMP_OUT'  'BUKRS'    'Empresa'         '07' '' ''   ''   '',
        "08  'BSIS'         'BELNR_FI'     'TG_DOCS_COMP_OUT'  'BELNR_FI' 'Doc.Ctb.'        '10' '' ''   ''   'X',
        09  'BSIS'         'GJAHR'        'TG_DOCS_COMP_OUT'  'GJAHR'    'Ano'             '04' '' ''   ''   '',
        "10  'BSIS'         'BUZEI'        'TG_DOCS_COMP_OUT'  'BUZEI'    'It.Ctb.'         '07' '' ''   ''   '',
        11  'MKPF'         'REFBN'        'TG_DOCS_COMP_OUT'  'REFBN'    'Doc.Ref.'        '10' '' ''   ''   'X'.
  "12  'MKPF'         'REFGJ'        'TG_DOCS_COMP_OUT'  'REFGJ'    'Ano.Ref'         '07' '' ''   ''   ' '.


  ls_variant-report = 'ZIMP60' && 'COMP'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = estrutura[]
      i_callback_user_command = 'USER_COMMAND_COMP'
      i_save                  = 'X'
      is_variant              = ls_variant
      i_screen_start_column   = 3
      i_screen_start_line     = 3
      i_screen_end_column     = 80
      i_screen_end_line       = 15
    TABLES
      t_outtab                = tg_docs_comp_out.

ENDFORM.


FORM user_command_comp USING r_ucomm     LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'BELNR_FI').

        READ TABLE tg_docs_comp_out INTO DATA(wl_doc_comp) INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_comp-belnr_fi IS NOT INITIAL ).

        SET PARAMETER ID 'BLN' FIELD wl_doc_comp-belnr_fi.
        SET PARAMETER ID 'BUK' FIELD wl_doc_comp-bukrs.
        SET PARAMETER ID 'GJR' FIELD wl_doc_comp-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'EBELN' ).

        READ TABLE tg_docs_comp_out INTO wl_doc_comp INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_comp-ebeln IS NOT INITIAL ).

        SET PARAMETER ID 'BES' FIELD wl_doc_comp-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'REFBN' ).

        READ TABLE tg_docs_comp_out INTO wl_doc_comp INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_comp-refbn IS NOT INITIAL ) AND ( wl_doc_comp-document_key IS NOT INITIAL ).

        CALL FUNCTION 'K_DOCUMENT_COMMITMENT_CALL'
          EXPORTING
            is_document_key = wl_doc_comp-document_key.


      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command

FORM user_command_compro USING r_ucomm     LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'EBELN' ).
        READ TABLE it_saida_comp INTO wa_saida_comp INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_comp-ebeln IS NOT INITIAL ).

        SET PARAMETER ID 'BES' FIELD wa_saida_comp-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command

*-CS2022001163-24.03.2023-#103539-JT-inicio
**********************************************
* movimnto pedido
**********************************************
FORM f_movimento_imobilizado   USING i_bukrs
                                     i_anln1
                                     i_anln2
                            CHANGING p_vlrbrl
                                     p_vlrusd.

  DATA : ls_variant TYPE disvariant,
         l_vlrbrl   TYPE kaep_coac-wkgbtr,
         l_vlrusd   TYPE kaep_coac-wkgbtr.

  FREE: t_sort,   it_saida_imob, it_fieldcat_imob.

  SELECT bukrs  anln1  anln2  lnran afabe  bwasl  bzdat   anbtr
    FROM anep
    INTO TABLE it_anep_imob
   WHERE bukrs EQ i_bukrs
     AND anln1 EQ i_anln1
     AND anln2 EQ i_anln2
     AND afabe EQ '01'
     AND bzdat IN p_data.

  SELECT bukrs  anln1  anln2  lnran afabe  bwasl  bzdat   anbtr
    FROM anep
    INTO TABLE it_anep_usd_imob
   WHERE bukrs EQ i_bukrs
     AND anln1 EQ i_anln1
     AND anln2 EQ i_anln2
     AND afabe EQ '41'
     AND bzdat IN p_data.

  SELECT *
    FROM tabwt
    INTO TABLE it_tabwt
     FOR ALL ENTRIES IN it_anep_imob
   WHERE spras = sy-langu
     AND bwasl = it_anep_imob-bwasl.

  SELECT *
    FROM tabwt
    APPENDING TABLE it_tabwt
     FOR ALL ENTRIES IN it_anep_usd_imob
   WHERE spras = sy-langu
     AND bwasl = it_anep_usd_imob-bwasl.

  FREE: l_vlrbrl, l_vlrusd.

  LOOP AT it_anep_imob INTO wa_anep_imob.

    CLEAR: wa_tabwt, wa_saida_imob.

    READ TABLE it_tabwt INTO wa_tabwt WITH KEY bwasl = wa_anep_imob-bwasl.

    l_vlrbrl                   = l_vlrbrl + wa_anep_imob-anbtr.

    wa_saida_imob-xtbrl_kob1   = wa_anep_imob-anbtr.
    wa_saida_imob-bwatxt       = wa_tabwt-bwatxt.
    wa_saida_imob-bzdat        = wa_anep_imob-bzdat.
    wa_saida_imob-bwasl        = wa_anep_imob-bwasl.
    COLLECT  wa_saida_imob  INTO it_saida_imob.

  ENDLOOP.

  LOOP AT it_anep_usd_imob INTO wa_anep_usd_imob.

    CLEAR: wa_tabwt, wa_saida_imob.

    READ TABLE it_tabwt INTO wa_tabwt WITH KEY bwasl = wa_anep_usd_imob-bwasl.

    l_vlrusd                   = l_vlrusd + wa_anep_usd_imob-anbtr.

    wa_saida_imob-xtusd_kob1   = wa_anep_usd_imob-anbtr.
    wa_saida_imob-bwatxt       = wa_tabwt-bwatxt.
    wa_saida_imob-bzdat        = wa_anep_usd_imob-bzdat.
    wa_saida_imob-bwasl        = wa_anep_usd_imob-bwasl.
    COLLECT  wa_saida_imob  INTO it_saida_imob.

  ENDLOOP.

  IF it_saida_imob[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foi encontrado Imobilizado!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  p_vlrbrl = l_vlrbrl.
  p_vlrusd = l_vlrusd.

  PERFORM preenche_cat_imob USING :
   '01' ''  '' 'IT_SAIDA_IMOB' 'BZDAT'            'Data'                  '25'     ''    ''     ''    '',
   '02' ''  '' 'IT_SAIDA_IMOB' 'BWASL'            'Tp.Mov'                '25'     ''    ''     ''    '',
   '03' ''  '' 'IT_SAIDA_IMOB' 'BWATXT'           'Descrição Tp.Mov'      '40'     ''    ''     ''    '',
   '04' ''  '' 'IT_SAIDA_IMOB' 'XTBRL_KOB1'       'Valor BRL'             '14'     ''    'X'    ''    '',
   '05' ''  '' 'IT_SAIDA_IMOB' 'XTUSD_KOB1'       'Valor USD'             '14'     ''    'X'    ''    ''.

  ls_variant-report = 'ZIMP60' && 'REAL'.

  CLEAR t_sort.
  t_sort-fieldname = 'BZDAT'.
  t_sort-subtot    = ' '.
  t_sort-spos      = 1.
  t_sort-up        = 'X'.
  APPEND t_sort.

  CLEAR t_sort.
  t_sort-fieldname = 'BWASL'.
  t_sort-subtot    = ' '.
  t_sort-spos      = 2.
  t_sort-up        = 'X'.
  APPEND t_sort.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat_imob[]
      it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_REAL'
      i_save                = 'X'
      is_variant            = ls_variant
      i_screen_start_column = 25
      i_screen_start_line   = 08
      i_screen_end_column   = 150
      i_screen_end_line     = 18
    TABLES
      t_outtab              = it_saida_imob.

ENDFORM.

**********************************************
* detalhes compromisso
**********************************************
FORM f_detalhes_compromisso    USING i_bukrs
                                     i_anln1
                                     i_anln2.

  DATA : ls_variant   TYPE disvariant,
         l_vlrbrl     TYPE kaep_coac-wkgbtr,
         l_vlrusd     TYPE kaep_coac-wkgbtr,
         tl_docs_real LIKE TABLE OF t_docs_real,
         wa_doc_real  LIKE t_docs_real,
         l_taxa       TYPE ukurs_curr.

  FREE: t_sort, it_saida_comp, it_fieldcat_comp.

  SELECT *
    INTO TABLE it_ekkn_comp
    FROM ekkn
   WHERE anln1 = i_anln1
     AND anln2 = i_anln2.

  FREE: tl_docs_real.
  LOOP AT it_ekkn_comp INTO wa_ekkn_comp WHERE anln1 = i_anln1
                                           AND anln2 = i_anln2.
    wa_doc_real-ebeln     = wa_ekkn_comp-ebeln.
    wa_doc_real-ebelp     = wa_ekkn_comp-ebelp.
    APPEND wa_doc_real   TO tl_docs_real.
  ENDLOOP.

  CLEAR: wa_saida_comp-xtbrl_kob2, wa_saida_comp-xtusd_kob2.

  IF tl_docs_real[] IS NOT INITIAL.
    SELECT ekpo~ebeln ekpo~ebelp ekko~waers ekpo~netpr
      INTO TABLE it_ekpo_comp
      FROM ekpo
     INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
       FOR ALL ENTRIES IN tl_docs_real
     WHERE ekpo~ebeln = tl_docs_real-ebeln
       AND ekpo~ebelp = tl_docs_real-ebelp
       AND ekpo~knttp = 'A'
       AND ekpo~loekz = abap_off.

    SELECT ebeln ebelp zekkn vgabe gjahr belnr buzei
      INTO TABLE it_ekbe_comp
      FROM ekbe
       FOR ALL ENTRIES IN tl_docs_real
     WHERE ebeln = tl_docs_real-ebeln
       AND ebelp = tl_docs_real-ebelp.

    SELECT *
      INTO TABLE it_eket_comp
      FROM eket
       FOR ALL ENTRIES IN tl_docs_real
     WHERE ebeln = tl_docs_real-ebeln
       AND ebelp = tl_docs_real-ebelp.
  ENDIF.

  LOOP AT tl_docs_real INTO wa_doc_real.

    CLEAR: wa_ekbe_comp, wa_eket_comp, wa_ekpo_comp.

    READ TABLE it_ekbe_comp INTO wa_ekbe_comp WITH KEY ebeln = wa_doc_real-ebeln
                                                       ebelp = wa_doc_real-ebelp.
    CHECK sy-subrc <> 0.

    READ TABLE it_eket_comp INTO wa_eket_comp WITH KEY ebeln = wa_doc_real-ebeln
                                                       ebelp = wa_doc_real-ebelp.
    CHECK sy-subrc = 0.

    READ TABLE it_ekpo_comp INTO wa_ekpo_comp WITH KEY ebeln = wa_doc_real-ebeln
                                                       ebelp = wa_doc_real-ebelp.
    CLEAR wa_saida_comp.

    PERFORM f_busca_taxa USING wa_eket_comp-eindt
                      CHANGING l_taxa.

    wa_saida_comp-bukrs   = i_bukrs.
    wa_saida_comp-waers   = wa_ekpo_comp-waers.
    wa_saida_comp-ebeln   = wa_ekpo_comp-ebeln.
    wa_saida_comp-ebelp   = wa_ekpo_comp-ebelp.

    IF wa_ekpo_comp-waers = 'BRL'.
      wa_saida_comp-xtbrl_kob2 = wa_ekpo_comp-netpr.
      wa_saida_comp-xtusd_kob2 = wa_ekpo_comp-netpr / l_taxa.
    ELSE.
      wa_saida_comp-xtusd_kob2 = wa_ekpo_comp-netpr.
      wa_saida_comp-xtbrl_kob2 = wa_ekpo_comp-netpr * l_taxa.
    ENDIF.

    APPEND wa_saida_comp TO it_saida_comp.
  ENDLOOP.

  IF it_saida_comp[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foi encontrado Compromisso!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM preenche_cat_comp USING :
   '01' ''  '' 'IT_SAIDA_COMP' 'BUKRS'            'Empresa'               '10'     ''    ''     ''    '',
   '02' ''  '' 'IT_SAIDA_COMP' 'WAERS'            'Moeda'                 '10'     ''    ''     ''    '',
   '03' ''  '' 'IT_SAIDA_COMP' 'EBELN'            'Pedido'                '10'     ''    ''     ''    'X',
   '04' ''  '' 'IT_SAIDA_COMP' 'EBELP'            'Item Pedido'           '12'     ''    ' '    ''    '',
   '05' ''  '' 'IT_SAIDA_COMP' 'XTBRL_KOB2'       'Valor BRL'             '14'     ''    'X'    ''    '',
   '06' ''  '' 'IT_SAIDA_COMP' 'XTUSD_KOB2'       'Valor USD'             '14'     ''    'X'    ''    ''.

  ls_variant-report = 'ZIMP60' && 'REAL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = it_fieldcat_comp[]
*     it_sort                 = t_sort[]
      i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_save                  = 'X'
      is_variant              = ls_variant
      i_screen_start_column   = 25
      i_screen_start_line     = 08
      i_screen_end_column     = 150
      i_screen_end_line       = 18
    TABLES
      t_outtab                = it_saida_comp.

ENDFORM.
*-CS2022001163-24.03.2023-#103539-JT-fim

FORM f_resumo_documentos_real USING p_saida  TYPE ty_saida.

  DATA : ls_variant TYPE disvariant.

  CHECK p_saida IS NOT INITIAL.

  CLEAR: tg_docs_real_out[], estrutura[].

  tg_docs_real_out[] = p_saida-docs_real[].

  CHECK tg_docs_real_out[] IS NOT INITIAL.

  SORT tg_docs_real_out BY budat DESCENDING.

  PERFORM f_montar_estrutura USING:
        01  'KAEP_COEP_X'  'AUFNR'        'TG_DOCS_REAL_OUT'  'AUFNR'    'Objeto'          '10' '' ''   ''   '',
        02  'COBK'         'BUDAT'        'TG_DOCS_REAL_OUT'  'BUDAT'    'Dt.Lcto'         '10' '' ''   ''   '',
        03  'BSIS'         'DMBTR'        'TG_DOCS_REAL_OUT'  'WOGBTR'   'Valor R$'        '13' '' 'X'  ''   '',
        04  'BSIS'         'DMBE2'        'TG_DOCS_REAL_OUT'  'WKGBTR'   'Valor U$'        '13' '' 'X'  ''   '',
        05  'EKPO'         'EBELN'        'TG_DOCS_REAL_OUT'  'EBELN'    'Pedido'          '10' '' ''   ''   'X',
        06  'EKPO'         'EBELP'        'TG_DOCS_REAL_OUT'  'EBELP'    'It.Ped.'         '06' '' ''   ''   '',
        07  'BSIS'         'BUKRS'        'TG_DOCS_REAL_OUT'  'BUKRS'    'Empresa'         '07' '' ''   ''   '',
        08  'BSIS'         'BELNR_FI'     'TG_DOCS_REAL_OUT'  'BELNR_FI' 'Doc.Ctb.'        '10' '' ''   ''   'X',
        09  'BSIS'         'GJAHR'        'TG_DOCS_REAL_OUT'  'GJAHR'    'Ano'             '04' '' ''   ''   '',
        10  'BSIS'         'BUZEI'        'TG_DOCS_REAL_OUT'  'BUZEI'    'It.Ctb.'         '07' '' ''   ''   '',
        11  'MKPF'         'REFBN'        'TG_DOCS_REAL_OUT'  'REFBN'    'Doc.Ref.'        '10' '' ''   ''   'X',
        12  'MKPF'         'REFGJ'        'TG_DOCS_REAL_OUT'  'REFGJ'    'Ano.Ref'         '07' '' ''   ''   ' '.

  ls_variant-report = 'ZIMP60' && 'REAL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = estrutura[]
      i_callback_user_command = 'USER_COMMAND_REAL'
      i_save                  = 'X'
      is_variant              = ls_variant
      i_screen_start_column   = 3
      i_screen_start_line     = 3
      i_screen_end_column     = 130
      i_screen_end_line       = 15
    TABLES
      t_outtab                = tg_docs_real_out.

ENDFORM.


FORM user_command_real USING r_ucomm     LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'BELNR_FI').

        READ TABLE tg_docs_real_out INTO DATA(wl_doc_real) INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_real-belnr_fi IS NOT INITIAL ).

        SET PARAMETER ID 'BLN' FIELD wl_doc_real-belnr_fi.
        SET PARAMETER ID 'BUK' FIELD wl_doc_real-bukrs.
        SET PARAMETER ID 'GJR' FIELD wl_doc_real-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'EBELN' ).

        READ TABLE tg_docs_real_out INTO wl_doc_real INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_real-ebeln IS NOT INITIAL ).

        SET PARAMETER ID 'BES' FIELD wl_doc_real-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'REFBN' ).

        READ TABLE tg_docs_real_out INTO wl_doc_real INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_real-refbn IS NOT INITIAL ) AND ( wl_doc_real-document_key IS NOT INITIAL ).

        CALL FUNCTION 'K_DOCUMENT_ACTUAL_CALL'
          EXPORTING
            is_document_key = wl_doc_real-document_key
            i_valutyp       = 0.


      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command


FORM f_resumo_documentos_adto USING p_saida  TYPE ty_saida.

  DATA : ls_variant TYPE disvariant.

  CHECK p_saida IS NOT INITIAL.

  CLEAR: tg_docs_adto_out[], estrutura[].

  tg_docs_adto_out = p_saida-docs_adto.

  CHECK tg_docs_adto_out[] IS NOT INITIAL.

  SORT tg_docs_adto_out BY budat DESCENDING.

  PERFORM f_montar_estrutura USING:
        01  'EKBE'    'EBELN'     'TG_DOCS_ADTO_OUT'  'EBELN'       'Pedido'          '10' '' ''   ''   'X',
        02  'EKBE'    'EBELP'     'TG_DOCS_ADTO_OUT'  'EBELP'       'It.Ped.'         '07' '' ''   ''   '',
        03  'EKBE'    'VGABE'     'TG_DOCS_ADTO_OUT'  'VGABE'       'Tp.Opr.'         '07' '' ''   ''   '',
        04  ''        ''          'TG_DOCS_ADTO_OUT'  'DS_OPERACAO' 'Operação'        '25' '' ''   'C'  '',
        05  'BSIK'    'BUKRS'     'TG_DOCS_ADTO_OUT'  'BUKRS'       'Empresa'         '07' '' ''   ''   '',
        06  'BSIK'    'BELNR'     'TG_DOCS_ADTO_OUT'  'BELNR'       'Documento'       '10' '' ''   ''   'X',
        07  'BSIK'    'GJAHR'     'TG_DOCS_ADTO_OUT'  'GJAHR'       'Ano'             '04' '' ''   ''   '',
        08  'BSIK'    'BUZEI'     'TG_DOCS_ADTO_OUT'  'BUZEI'       'It.Doc.'         '07' '' ''   ''   '',
        09  'BSIK'    'DMBTR'     'TG_DOCS_ADTO_OUT'  'DMBTR'       'Valor R$'        '13' '' 'X'  ''   '',
        10  'BSIK'    'DMBE2'     'TG_DOCS_ADTO_OUT'  'DMBE2'       'Valor U$'        '13' '' 'X'  ''   '',
        11  'BSIK'    'BUDAT'     'TG_DOCS_ADTO_OUT'  'BUDAT'       'Dt.Lcto'         '10' '' ''   ''   ''.

  ls_variant-report = 'ZIMP60' && 'ADTO'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      "IS_LAYOUT               = GS_LAYOUT
      it_fieldcat             = estrutura[]
      i_callback_user_command = 'USER_COMMAND_ADTO'
      i_save                  = 'X'
      is_variant              = ls_variant
      i_screen_start_column   = 3
      i_screen_start_line     = 3
      i_screen_end_column     = 130
      i_screen_end_line       = 15
    TABLES
      t_outtab                = tg_docs_adto_out.

ENDFORM.


FORM user_command_adto USING r_ucomm     LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'BELNR').

        READ TABLE tg_docs_adto_out INTO DATA(wl_doc_adto) INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_adto-belnr IS NOT INITIAL ).

        SET PARAMETER ID 'BLN' FIELD wl_doc_adto-belnr.
        SET PARAMETER ID 'BUK' FIELD wl_doc_adto-bukrs.
        SET PARAMETER ID 'GJR' FIELD wl_doc_adto-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ELSEIF ( rs_selfield-fieldname EQ 'EBELN' ).

        READ TABLE tg_docs_adto_out INTO wl_doc_adto INDEX rs_selfield-tabindex.
        CHECK ( sy-subrc EQ 0 ) AND ( wl_doc_adto-ebeln IS NOT INITIAL ).

        SET PARAMETER ID 'BES' FIELD wl_doc_adto-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
      VALUE(p_ref_tabname)   LIKE dd02d-tabname
      VALUE(p_ref_fieldname) LIKE dd03d-fieldname
      VALUE(p_tabname)       LIKE dd02d-tabname
      VALUE(p_field)         LIKE dd03d-fieldname
      VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
      VALUE(p_outputlen)
      VALUE(p_edit)
      VALUE(p_do_sum)
      VALUE(p_just)
      VALUE(p_hotspot).

  CLEAR: wa_estrutura.

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
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.

FORM f_config_color_line CHANGING c_saida TYPE ty_saida.

  DATA: wl_color  TYPE kkblo_specialcol.

*--------------------------------------------------------------------------*
* Adiantamento
*--------------------------------------------------------------------------*
  IF c_saida-docs_adto[] IS NOT INITIAL.
    CLEAR: wl_color.

    wl_color-color-col = 3.
    wl_color-color-int = 0.
    wl_color-color-inv = 0.

    wl_color-fieldname = 'XTOAD_BRL'.
    APPEND wl_color TO c_saida-color.

    wl_color-fieldname = 'XTOAD_USD'.
    APPEND wl_color TO c_saida-color.
  ENDIF.


*--------------------------------------------------------------------------*
* Compromissado
*--------------------------------------------------------------------------*
  IF c_saida-docs_comp[] IS NOT INITIAL.
    CLEAR: wl_color.

    wl_color-color-col = 7.
    wl_color-color-int = 0.
    wl_color-color-inv = 0.

    wl_color-fieldname = 'XTBRL_KOB2'.
    APPEND wl_color TO c_saida-color.

    wl_color-fieldname = 'XTUSD_KOB2'.
    APPEND wl_color TO c_saida-color.
  ENDIF.

*--------------------------------------------------------------------------*
* Realizado
*--------------------------------------------------------------------------*
  IF c_saida-docs_real[] IS NOT INITIAL.
    CLEAR: wl_color.

    wl_color-color-col = 5.
    wl_color-color-int = 0.
    wl_color-color-inv = 0.

    wl_color-fieldname = 'XTBRL_KOB1'.
    APPEND wl_color TO c_saida-color.

    wl_color-fieldname = 'XTUSD_KOB1'.
    APPEND wl_color TO c_saida-color.
  ENDIF.

ENDFORM.

FORM f_salvar_dados.
  DATA: ls_aufnr  TYPE aufnr.
  DATA(_gravou) = abap_false.


  LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    CLEAR: wa_zim13.

    IF ( <fs_saida>-dt_fim IS NOT INITIAL ) AND ( <fs_saida>-dt_inicio IS INITIAL ).
      MESSAGE |Empresa: { <fs_saida>-bukrs } / Ano: { <fs_saida>-ano } / Ordem: { <fs_saida>-posnr ALPHA = OUT  } sem DATA Inicio. Não poder ser informado DATA Fim! | TYPE 'I'.
      CLEAR: <fs_saida>-dt_fim.
    ENDIF.

    IF ( <fs_saida>-dt_inicio IS NOT INITIAL ) AND ( <fs_saida>-dt_fim IS NOT INITIAL ).
      IF <fs_saida>-dt_fim < <fs_saida>-dt_inicio.
        MESSAGE |Empresa: { <fs_saida>-bukrs } / Ano: { <fs_saida>-ano } / Ordem: { <fs_saida>-posnr ALPHA = OUT  }. DATA Fim menor que DATA de Inicio! | TYPE 'I'.
        CLEAR: <fs_saida>-dt_fim.
      ENDIF.
    ENDIF.

    CLEAR: ls_aufnr.
    ls_aufnr = <fs_saida>-objto.
    ls_aufnr = |{ ls_aufnr ALPHA =  IN }|.

    wa_zim13-status   = <fs_saida>-status.
    wa_zim13-bukrs    = <fs_saida>-bukrs.
    wa_zim13-ano      = <fs_saida>-ano.
    wa_zim13-posnr    = |{ <fs_saida>-posnr ALPHA =  IN }|.
    wa_zim13-aufnr    = |{ <fs_saida>-objto ALPHA =  IN }|.
    wa_zim13-oberv    = <fs_saida>-texto.
    wa_zim13-dt_fim   = <fs_saida>-dt_fim.
    wa_zim13-usuario  = sy-uname.
    wa_zim13-dt_modf  = sy-datum.
    wa_zim13-hr_modf  = sy-uzeit.

    SELECT SINGLE * FROM  imaka INTO @DATA(wimaka) WHERE posnr EQ  @wa_zim13-posnr.
    IF sy-subrc = 0.
      wa_zim13-anln1 = wimaka-anln1.
      wa_zim13-anln2 = wimaka-anln2.
    ENDIF.

    MODIFY zim13 FROM wa_zim13.

    COMMIT WORK. "US - 62758 - CBRAND

    _gravou = abap_true.

  ENDLOOP.

  IF _gravou EQ abap_true.
    MESSAGE 'Dados gravados com sucesso!' TYPE 'I'.
  ENDIF.

  "LEAVE TO SCREEN 0100.

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

************************************************************************
* buscar taxa dolar
************************************************************************
FORM f_busca_taxa   USING p_data
                 CHANGING p_taxa.

  DATA: l_gdatu TYPE gdatu_inv.

  DATA(obj_zcl_util_sd) = NEW zcl_util_sd( ).

  FREE: p_taxa.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').

  MOVE p_data TO l_gdatu.

  obj_zcl_util_sd->set_data( l_gdatu ).
  p_taxa = obj_zcl_util_sd->taxa_cambio( ).

ENDFORM.                    " BUSCA_TAXA

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ZIMP60'.
  SET TITLEBAR 'ZIMP60'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: l_valor_brl TYPE kaep_coac-wkgbtr,
        l_valor_usd TYPE kaep_coac-wkgbtr.

  CASE ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'OK'.
      CASE abap_true.
        WHEN bot_mvp.
          "Imobilizado.
          PERFORM f_resumo_documentos_real   USING wa_saida.

*         wa_saida-xtbrl_kob1 = wa_saida-xtbrl_kob1_ori.
*         wa_saida-xtusd_kob1 = wa_saida-xtusd_kob1_ori.
*         MODIFY it_saida FROM wa_saida INDEX l_row_saida.

        WHEN bot_mvi.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_saida-objto
            IMPORTING
              output = l_objeto.

          PERFORM f_movimento_imobilizado    USING wa_saida-bukrs
                                                   l_objeto
                                                   wa_saida-anln2
                                          CHANGING l_valor_brl    "wa_saida-xtbrl_kob1
                                                   l_valor_usd.   "wa_saida-xtusd_kob1.
*         MODIFY it_saida FROM wa_saida INDEX l_row_saida.
      ENDCASE.
  ENDCASE.

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  LEAVE TO SCREEN 0.

ENDMODULE.
