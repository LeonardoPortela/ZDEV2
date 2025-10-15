FUNCTION z_sample_process_01000710.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_XEBAN STRUCTURE  EBAN OPTIONAL
*"      T_XEBKN STRUCTURE  EBKN OPTIONAL
*"      T_YEBAN STRUCTURE  EBAN OPTIONAL
*"      T_YEBKN STRUCTURE  EBKN OPTIONAL
*"----------------------------------------------------------------------
*  DATA:  T_ZMM_APROV_RCC  TYPE ZMM_APROV_RCC,
*         TABIX            TYPE SY-TABIX.
*
*  IF SY-CPROG = 'ZMMR047'.
*    LOOP AT T_XEBAN.
*      TABIX = SY-TABIX.
*      SELECT SINGLE *
*        FROM ZMM_APROV_RCC
*        INTO T_ZMM_APROV_RCC
*        WHERE MATNR EQ T_XEBAN-MATNR.
*
*      MOVE T_ZMM_APROV_RCC-FRGST TO T_XEBAN-FRGST.
*      MODIFY T_XEBAN INDEX TABIX TRANSPORTING FRGST.
*    ENDLOOP.
*
*  ENDIF.


ENDFUNCTION.
