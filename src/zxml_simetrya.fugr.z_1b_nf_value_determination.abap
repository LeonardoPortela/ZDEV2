FUNCTION z_1b_nf_value_determination.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_DOCNUM) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     VALUE(EXT_HEADER) LIKE  J_1BINDOC STRUCTURE  J_1BINDOC
*"  TABLES
*"      EXT_ITEM STRUCTURE  J_1BINLIN OPTIONAL
*"      EXT_TOTAL_TAX STRUCTURE  J_1BINTAX OPTIONAL
*"----------------------------------------------------------------------

*.................. local data ....................................... *

  CONSTANTS: c_icms LIKE j_1baj-taxgrp VALUE 'ICMS',
             c_ipi  LIKE j_1baj-taxgrp VALUE 'IPI',
             c_icst LIKE j_1baj-taxgrp VALUE 'ICST',
             c_icfr LIKE j_1baj-taxgrp VALUE 'ICFR',
             c_icfs LIKE j_1baj-taxgrp VALUE 'ICFS',
             c_iss  LIKE j_1baj-taxgrp VALUE 'ISS',
             c_issp LIKE j_1baj-taxgrp VALUE 'ISSP',
             c_isss LIKE j_1baj-taxgrp VALUE 'ISSS',
             c_pis    LIKE j_1baj-taxgrp VALUE 'PIS',
             c_cofins LIKE j_1baj-taxgrp VALUE 'COFI',
             c_whpi LIKE j_1baj-taxgrp VALUE 'WHPI',
             c_whco LIKE j_1baj-taxgrp VALUE 'WHCO',
             c_whcs LIKE j_1baj-taxgrp VALUE 'WHCS',
             c_whir LIKE j_1baj-taxgrp VALUE 'WHIR'.

  DATA: nf_header TYPE j_1bnfdoc.
  DATA: tax_types LIKE j_1baj    OCCURS 10 WITH HEADER LINE.



  DATA: nf_item     TYPE TABLE OF j_1bnflin INITIAL SIZE 0 WITH HEADER LINE,
        nf_item_tax TYPE TABLE OF j_1bnfstx INITIAL SIZE 0 WITH HEADER LINE.


  DATA: tmp_tax     LIKE j_1bnfstx OCCURS 10 WITH HEADER LINE.

  SELECT SINGLE * INTO nf_header
    FROM j_1bnfdoc
   WHERE docnum EQ p_docnum.

  SELECT *
    FROM j_1bnflin
    INTO TABLE nf_item
    WHERE docnum EQ p_docnum.

  select *
    from j_1bnfstx
    into table nf_item_tax
    where docnum eq p_docnum.

* begin change 16.01.97: exclude statistical items/taxes
*.................. data related to 'statistical' values ............. *

  DATA: BEGIN OF ext_total_stat_tax OCCURS 0.
          INCLUDE STRUCTURE j_1bintax.
  DATA: END OF ext_total_stat_tax.
  "tax totals for statistical taxes
  DATA: BEGIN OF ext_header_stat OCCURS 0.
          INCLUDE STRUCTURE j_1bindoc.
  DATA: END OF ext_header_stat.
  "doc totals including also statistical
  "taxes (ext_header table includes NO
  "statistical taxes!)
* end change 16.01.97: exclude statistical items/taxes

*ENHANCEMENT-POINT J_1B_NF_VALUE_DETERMINATION_01 SPOTS ES_SAPLJ1BC.
*.................. preprocessing .................................... *

  CLEAR    ext_header.
  REFRESH: ext_item,
           ext_total_tax.

* get tax type - tax group assigment
  DESCRIBE TABLE tax_types LINES sy-tfill.
  IF sy-tfill = 0.
    SELECT * FROM j_1baj INTO TABLE tax_types ORDER BY PRIMARY KEY.
  ENDIF.

*.................. build EXT_TOTAL_TAX .............................. *

  CLEAR ext_total_tax.
  ext_total_tax-docnum = nf_header-docnum.
*ENHANCEMENT-SECTION     J_1B_NF_VALUE_DETERMINATION_02 SPOTS ES_SAPLJ1BC.
  LOOP AT nf_item_tax.
*   get service flag from item                               "NFe
    READ TABLE nf_item WITH KEY docnum = nf_item_tax-docnum  "NFe
                                itmnum = nf_item_tax-itmnum. "NFe
    IF NOT sy-subrc IS INITIAL.
      CLEAR nf_item-tmiss.
    ENDIF.

    IF nf_item_tax-stattx = ' '.  "change 16.01.97: excl. stat. txs/itms
      MOVE-CORRESPONDING nf_item_tax TO ext_total_tax.
* for service create additional sums / necessary in mixed NFs
      IF nf_item-tmiss = 'X'.                                    "NFe
        MOVE: nf_item_tax-base   TO ext_total_tax-baseser,       "NFe
              nf_item_tax-taxval TO ext_total_tax-taxvalser,     "NFe
              nf_item_tax-excbas TO ext_total_tax-excbasser,     "NFe
              nf_item_tax-othbas TO ext_total_tax-othbasser.     "NFe
      ELSE.                                                      "NFe
        CLEAR: ext_total_tax-baseser,                            "NFe
               ext_total_tax-taxvalser,                          "NFe
               ext_total_tax-excbasser,                          "NFe
               ext_total_tax-othbasser.                          "NFe
      ENDIF.                                                     "NFe
      COLLECT ext_total_tax.
* begin change 16.01.97: exclude statistical items/taxes
    ELSE.
      MOVE-CORRESPONDING nf_item_tax TO ext_total_stat_tax.
* for service create additional sums / necessary in mixed NFs
      IF nf_item-tmiss = 'X'.                                     "NFe
        MOVE: nf_item_tax-base   TO ext_total_stat_tax-baseser,   "NFe
              nf_item_tax-taxval TO ext_total_stat_tax-taxvalser, "NFe
              nf_item_tax-excbas TO ext_total_stat_tax-excbasser, "NFe
              nf_item_tax-othbas TO ext_total_stat_tax-othbasser. "NFe
      ELSE.                                                       "NFe
        CLEAR: ext_total_tax-baseser,                             "NFe
               ext_total_tax-taxvalser,                           "NFe
               ext_total_tax-excbasser,                           "NFe
               ext_total_tax-othbasser.                           "NFe
      ENDIF.                                                      "NFe
      COLLECT ext_total_stat_tax.
    ENDIF.
* end change 16.01.97: exclude statistical items/taxes
  ENDLOOP.
*END-ENHANCEMENT-SECTION.
  SORT ext_total_tax BY docnum taxtyp.
  SORT ext_total_stat_tax BY docnum taxtyp. "change 16.01.97: excl. stat

*.................. build EXT_ITEM ................................... *
  DATA: lv_tabix TYPE sy-tabix.

  LOOP AT nf_item.
    lv_tabix = sy-tabix.
*   get taxes for one item
    REFRESH tmp_tax. CLEAR tmp_tax.
    LOOP AT nf_item_tax WHERE mandt  = nf_item-mandt
                          AND docnum = nf_item-docnum
                          AND itmnum = nf_item-itmnum.
*ENHANCEMENT-SECTION     J_1B_NF_VALUE_DETERMINATION_03 SPOTS ES_SAPLJ1BC.
      tmp_tax = nf_item_tax.
      APPEND tmp_tax.
*END-ENHANCEMENT-SECTION.
    ENDLOOP.

    CALL FUNCTION 'J_1B_NF_VALUE_DETERMINATION_I'
      EXPORTING
        nf_item     = nf_item
      IMPORTING
        ext_item    = ext_item
      TABLES
        nf_item_tax = tmp_tax.

*   store EXT_ITEM
    APPEND ext_item.

    MOVE: ext_item-nfpri TO nf_item-nfpri,
          ext_item-nfnet TO nf_item-nfnet,
          ext_item-nfdis TO nf_item-nfdis,
          ext_item-nffre TO nf_item-nffre,
          ext_item-nfins TO nf_item-nfins,
          ext_item-nfoth TO nf_item-nfoth,
          ext_item-netwrt TO nf_item-netwrt,
          ext_item-nfnett TO nf_item-nfnett.

    MODIFY nf_item INDEX lv_tabix.
  ENDLOOP.

*.................. build EXT_HEADER ................................. *

  CLEAR ext_header.

  ext_header-docnum = nf_header-docnum.
* calculate tax totals for N.F. - change 16.01.97: without stat. taxes
  LOOP AT ext_total_tax.
*   get taxgroup for taxtype
    READ TABLE tax_types WITH KEY taxtyp = ext_total_tax-taxtyp.
*   get base and value for ICMS / Sub.Trib / IPI
    IF sy-subrc = 0.
*ENHANCEMENT-SECTION     J_1B_NF_VALUE_DETERMINATION_04 SPOTS ES_SAPLJ1BC.
      IF tax_types-taxgrp = c_icms.
        ADD ext_total_tax-base   TO ext_header-icmsbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ICMSBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-icmsval.
      ELSEIF tax_types-taxgrp = c_icst.
        ADD ext_total_tax-base   TO ext_header-icstbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ICSTBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-icstval.
      ELSEIF tax_types-taxgrp = c_ipi.
        ADD ext_total_tax-base   TO ext_header-ipibase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-IPIBASE.   " note 779931
        ADD ext_total_tax-taxval TO ext_header-ipival.
      ELSEIF tax_types-taxgrp = c_icfr.
        ADD ext_total_tax-base   TO ext_header-icfrbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ICFRBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-icfrval.
      ELSEIF tax_types-taxgrp = c_icfs.
        ADD ext_total_tax-base   TO ext_header-icfsbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ICFSBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-icfsval.
      ELSEIF tax_types-taxgrp = c_issp.
        ADD ext_total_tax-base   TO ext_header-isspbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ISSPBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-isspval.
        ADD ext_total_tax-excbasser TO ext_header-isspexcbser. "NFe
        ADD ext_total_tax-othbasser TO ext_header-isspothbser. "NFe
        ADD ext_total_tax-baseser   TO ext_header-isspbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header-isspvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_isss.
        ADD ext_total_tax-base   TO ext_header-isssbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-ISSSBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-isssval.
        ADD ext_total_tax-excbasser TO ext_header-isssexcbser. "NFe
        ADD ext_total_tax-othbasser TO ext_header-isssothbser. "NFe
        ADD ext_total_tax-baseser   TO ext_header-isssbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header-isssvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_pis.
        ADD ext_total_tax-base   TO ext_header-pisbase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-PISBASE.   " note 779931
        ADD ext_total_tax-taxval TO ext_header-pisval.
        ADD ext_total_tax-baseser   TO ext_header-pisbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header-pisvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_cofins.
        ADD ext_total_tax-base   TO ext_header-cofibase.
*        ADD EXT_TOTAL_TAX-OTHBAS TO EXT_HEADER-COFIBASE.  " note 779931
        ADD ext_total_tax-taxval TO ext_header-cofival.
        ADD ext_total_tax-baseser   TO ext_header-cofibaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header-cofivalser.  "NFe
*
* ==> following tax types are for NFe
*
      ELSEIF tax_types-taxgrp = c_whpi.
        ADD ext_total_tax-base   TO ext_header-pisbasewht.
        ADD ext_total_tax-taxval TO ext_header-pisvalwht.
      ELSEIF tax_types-taxgrp = c_whco.
        ADD ext_total_tax-base   TO ext_header-cofibasewht.
        ADD ext_total_tax-taxval TO ext_header-cofivalwht.
      ELSEIF tax_types-taxgrp = c_whcs.
        ADD ext_total_tax-base   TO ext_header-csslbasewht.
        ADD ext_total_tax-taxval TO ext_header-csslvalwht.
      ELSEIF tax_types-taxgrp = c_whir.
        ADD ext_total_tax-base   TO ext_header-irrfbasewht.
        ADD ext_total_tax-taxval TO ext_header-irrfvalwht.
      ENDIF.
*END-ENHANCEMENT-SECTION.
    ENDIF.
  ENDLOOP.

* begin change 16.01.97: exclude statistical items/taxes
  ext_header_stat-docnum = nf_header-docnum.
* calculate tax totals for N.F.  - only statistical taxes
  LOOP AT ext_total_stat_tax.
*   get taxgroup for taxtype
    READ TABLE tax_types WITH KEY taxtyp = ext_total_stat_tax-taxtyp.
*   get base and value for ICMS / Sub.Trib / IPI
    IF sy-subrc = 0.
*ENHANCEMENT-SECTION     J_1B_NF_VALUE_DETERMINATION_05 SPOTS ES_SAPLJ1BC.
* note 779931: No consideration of 'other base' in 'ICMS base'
      IF tax_types-taxgrp = c_icms.
        ADD ext_total_stat_tax-base   TO ext_header_stat-icmsbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ICMSBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-icmsval.
      ELSEIF tax_types-taxgrp = c_icst.
        ADD ext_total_stat_tax-base   TO ext_header_stat-icstbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ICSTBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-icstval.
      ELSEIF tax_types-taxgrp = c_ipi.
        ADD ext_total_stat_tax-base   TO ext_header_stat-ipibase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-IPIBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-ipival.
      ELSEIF tax_types-taxgrp = c_icfr.
        ADD ext_total_stat_tax-base   TO ext_header_stat-icfrbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ICFRBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-icfrval.
      ELSEIF tax_types-taxgrp = c_icfs.
        ADD ext_total_stat_tax-base   TO ext_header_stat-icfsbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ICFSBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-icfsval.
      ELSEIF tax_types-taxgrp = c_issp.
        ADD ext_total_stat_tax-base   TO ext_header_stat-isspbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ISSPBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-isspval.
        ADD ext_total_tax-excbasser TO ext_header_stat-isspexcbser. "NFe
        ADD ext_total_tax-othbasser TO ext_header_stat-isspothbser. "NFe
        ADD ext_total_tax-baseser   TO ext_header_stat-isspbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header_stat-isspvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_isss.
        ADD ext_total_stat_tax-base   TO ext_header_stat-isssbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-ISSSBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-isssval.
        ADD ext_total_tax-excbasser TO ext_header_stat-isssexcbser. "NFe
        ADD ext_total_tax-othbasser TO ext_header_stat-isssothbser. "NFe
        ADD ext_total_tax-baseser   TO ext_header_stat-isssbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header_stat-isssvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_pis.
        ADD ext_total_stat_tax-base   TO ext_header_stat-pisbase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-PISBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-pisval.
        ADD ext_total_tax-baseser   TO ext_header_stat-pisbaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header_stat-pisvalser.  "NFe
      ELSEIF tax_types-taxgrp = c_cofins.
        ADD ext_total_stat_tax-base   TO ext_header_stat-cofibase.
*        ADD EXT_TOTAL_STAT_TAX-OTHBAS TO EXT_HEADER_STAT-COFIBASE.
        ADD ext_total_stat_tax-taxval TO ext_header_stat-cofival.
        ADD ext_total_tax-baseser   TO ext_header_stat-cofibaseser. "NFe
        ADD ext_total_tax-taxvalser TO ext_header_stat-cofivalser.  "NFe
*
* ==> following tax types are for NFe
*
      ELSEIF tax_types-taxgrp = c_whpi.
        ADD ext_total_tax-base   TO ext_header_stat-pisbasewht.
        ADD ext_total_tax-taxval TO ext_header_stat-pisvalwht.
      ELSEIF tax_types-taxgrp = c_whco.
        ADD ext_total_tax-base   TO ext_header_stat-cofibasewht.
        ADD ext_total_tax-taxval TO ext_header_stat-cofivalwht.
      ELSEIF tax_types-taxgrp = c_whcs.
        ADD ext_total_tax-base   TO ext_header_stat-csslbasewht.
        ADD ext_total_tax-taxval TO ext_header_stat-csslvalwht.
      ELSEIF tax_types-taxgrp = c_whir.
        ADD ext_total_tax-base   TO ext_header_stat-irrfbasewht.
        ADD ext_total_tax-taxval TO ext_header_stat-irrfvalwht.
      ENDIF.
*END-ENHANCEMENT-SECTION.
    ENDIF.
  ENDLOOP.
* end change 16.01.97: exclude statistical items/taxes

* calculate totals of NF values including ICMS
  LOOP AT ext_item.
    READ TABLE nf_item WITH KEY docnum = ext_item-docnum
                                itmnum = ext_item-itmnum.
    IF nf_item-statit = ' '.        "change 20.01.97: excl. stat. item
      ADD ext_item-nfnett TO ext_header-nfnett.
      ADD ext_item-nfnet  TO ext_header-nfnet.
      ADD ext_item-nffre  TO ext_header-nffre.
      ADD ext_item-nfins  TO ext_header-nfins.
      ADD ext_item-nfoth  TO ext_header-nfoth.
      ADD ext_item-nfdis  TO ext_header-nfdis.
    ENDIF.                          "change 20.01.97: excl. stat. item
  ENDLOOP.

* calculate total of N.F = NFNET+NFFRE+NFINS+NFOTH+Total IPI+Total S.T.
  ext_header-nftot =   ext_header-nfnet
                     + ext_header-nffre
                     + ext_header-nfins
                     + ext_header-nfoth
                     + ext_header-nfdis
                     + ext_header-ipival
                     + ext_header-icstval
                     + ext_header-icfrval
                     + ext_header-icfsval
                     - nf_header-witha "change 15.01.97: withholding tax
                     + ext_header_stat-ipival  "change 16.01.97: exclude
                     + ext_header_stat-icstval." statistical items/taxes

*ENHANCEMENT-POINT J_1B_NF_VALUE_DETERMINATION_06 SPOTS ES_SAPLJ1BC.
ENDFUNCTION.
