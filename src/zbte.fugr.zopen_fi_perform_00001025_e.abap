FUNCTION ZOPEN_FI_PERFORM_00001025_E.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BKDF) LIKE  BKDF STRUCTURE  BKDF OPTIONAL
*"  TABLES
*"      T_AUSZ1 STRUCTURE  AUSZ1 OPTIONAL
*"      T_AUSZ2 STRUCTURE  AUSZ2 OPTIONAL
*"      T_AUSZ3 STRUCTURE  AUSZ_CLR OPTIONAL
*"      T_BKP1 STRUCTURE  BKP1
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEC STRUCTURE  BSEC
*"      T_BSED STRUCTURE  BSED
*"      T_BSEG STRUCTURE  BSEG
*"      T_BSET STRUCTURE  BSET
*"      T_BSEU STRUCTURE  BSEU OPTIONAL
*"      T_RSGTAB STRUCTURE  IRSGTAB OPTIONAL
*"      T_RENUM STRUCTURE  IRENUM OPTIONAL
*"      T_POSTAB STRUCTURE  AUSZ_INFO OPTIONAL
*"--------------------------------------------------------------------



  message e000(zfi) with 'teste bte'.

*DATA: l_ausz1  TYPE TABLE OF ausz1,                    "start:note530655
*      l_ausz2  TYPE TABLE OF ausz2,
*      l_ausz3  TYPE TABLE OF ausz_clr,
*      l_bkpf   TYPE TABLE OF bkpf,
*      l_bkp1   TYPE TABLE OF bkp1,
*      l_bsec   TYPE TABLE OF bsec,
*      l_bsed   TYPE TABLE OF bsed,
*      l_bseg   TYPE TABLE OF bseg,
*      l_bset   TYPE TABLE OF bset,
*      l_bseu   TYPE TABLE OF bseu.                       "end:note530655
*DATA: l_rsgtab TYPE TABLE OF irsgtab,                        "note561898
*      l_renum  TYPE TABLE OF irenum,                         "note561898
*      l_postab TYPE TABLE OF ausz_info.                      "note561898
*
*  DATA: ld_t001 LIKE t001.
*  DATA: ld_t005 LIKE t005.
*  CLEAR gd_intca.
**-- Performance simulate country filter for SAP components
*  LOOP AT t_bkpf.
*    IF ld_t001-bukrs NE t_bkpf-bukrs.
*      CALL FUNCTION 'FI_COMPANY_CODE_DATA'
*        EXPORTING
*          i_bukrs = t_bkpf-bukrs
*        IMPORTING
*          e_t001  = ld_t001.
*      IF ld_t001-land1 NE ld_t005-land1
*      AND ld_t005-land1 EQ space.
*        CALL FUNCTION 'FI_COUNTRY_DATA'
*          EXPORTING
*            i_land1 = ld_t001-land1
*          IMPORTING
*            e_t005  = ld_t005.
*        EXIT.
*      ENDIF.
*    ENDIF.
** provide country of sending company code - ignore other cc in T_BKPF
*    EXIT.
*  ENDLOOP.
**save T005-intca for later cal of BTE 1030/ 1050
*  gd_intca = ld_t005-intca.
*  CALL FUNCTION 'BF_FUNCTIONS_FIND'
*    EXPORTING
*      i_event       = '00001025'
*      i_intca       = gd_intca
*    TABLES
*      t_fmrfc       = fmtab
*    EXCEPTIONS
*      nothing_found = 4
*      OTHERS        = 8.
*  CHECK sy-subrc = 0.
*
**------------------ Save interface data --------------------------------
**  MEMID+6 = '00001025E'.                             "start:note530655
**  EXPORT T_AUSZ1 T_AUSZ2 T_AUSZ3 T_BKPF T_BKP1
**         T_BSEC T_BSED T_BSEG  T_BSET  T_BSEU
**         TO MEMORY ID MEMID.
*  l_ausz1[] = t_ausz1[].
*  l_ausz2[] = t_ausz2[].
*  l_ausz3[] = t_ausz3[].
*  l_bkpf[]  = t_bkpf[].
*  l_bkp1[]  = t_bkp1[].
*  l_bsec[]  = t_bsec[].
*  l_bsed[]  = t_bsed[].
*  l_bseg[]  = t_bseg[].
*  l_bset[]  = t_bset[].
*  l_bseu[]  = t_bseu[].                                  "end:note530655
*  l_rsgtab[] = t_rsgtab[].                                   "note561898
*  l_renum[]  = t_renum[].                                    "note561898
*  l_postab[] = t_postab[].                                   "note561898
*
*  LOOP AT fmtab.
*    CHECK NOT fmtab-funct IS INITIAL.
*    IF fmtab-rfcds IS INITIAL.
*
**------------- Open FI Interface with local destination ----------------
*      CALL FUNCTION fmtab-funct
*        EXPORTING
*          i_bkdf   = i_bkdf
*        TABLES
*          t_ausz1  = t_ausz1
*          t_ausz2  = t_ausz2
*          t_ausz3  = t_ausz3
*          t_bkpf   = t_bkpf
*          t_bkp1   = t_bkp1
*          t_bsec   = t_bsec
*          t_bsed   = t_bsed
*          t_bseg   = t_bseg
*          t_bset   = t_bset
*          t_bseu   = t_bseu
*          t_rsgtab = t_rsgtab                               "note561898
*          t_renum  = t_renum                                "note561898
*          t_postab = t_postab.                              "note561898
*
*    ELSE.
*
**------------- Open FI Interface with foreign destination --------------
*      CALL FUNCTION fmtab-funct
*        DESTINATION fmtab-rfcds
*        EXPORTING
*          i_bkdf                = i_bkdf
*        TABLES
*          t_ausz1               = t_ausz1
*          t_ausz2               = t_ausz2
*          t_ausz3               = t_ausz3
*          t_bkpf                = t_bkpf
*          t_bkp1                = t_bkp1
*          t_bsec                = t_bsec
*          t_bsed                = t_bsed
*          t_bseg                = t_bseg
*          t_bset                = t_bset
*          t_bseu                = t_bseu
*          t_rsgtab              = t_rsgtab                  "note561898
*          t_renum               = t_renum                   "note561898
*          t_postab              = t_postab                  "note561898
*        EXCEPTIONS
*          communication_failure = 1
*          system_failure        = 2.
*      IF sy-subrc NE 0.
*        MESSAGE e011 WITH fmtab-rfcds.
*      ENDIF.
*    ENDIF.
**    IMPORT T_AUSZ1 T_AUSZ2 T_AUSZ3 T_BKPF T_BKP1      "start:note530655
**           T_BSEC T_BSED T_BSEG  T_BSET  T_BSEU
**           FROM  MEMORY ID MEMID.
*    t_ausz1[] = l_ausz1[].
*    t_ausz2[] = l_ausz2[].
*    t_ausz3[] = l_ausz3[].
*    t_bkpf[]  = l_bkpf[].
*    t_bkp1[]  = l_bkp1[].
*    t_bsec[]  = l_bsec[].
*    t_bsed[]  = l_bsed[].
*    t_bseg[]  = l_bseg[].
*    t_bset[]  = l_bset[].
*    t_bseu[]  = l_bseu[].                                "end:note530655
*    t_rsgtab[] = l_rsgtab[].                                 "note561898
*    t_renum[]  = l_renum[].                                  "note561898
*    t_postab[] = l_postab[].                                 "note561898
*  ENDLOOP.
ENDFUNCTION.
