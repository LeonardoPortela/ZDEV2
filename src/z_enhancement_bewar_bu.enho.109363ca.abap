"Name: \PR:SAPMF05A\FO:FCODE_BEARBEITUNG_BU_BS\SE:END\EI
ENHANCEMENT 0 Z_ENHANCEMENT_BEWAR_BU.
 DATA: T_HKONT TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
       T_BUKRS TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE,
       T_ZFIT0030 TYPE TABLE OF ZFIT0030 WITH HEADER LINE,
       MSGE(50) TYPE C,
       FLAGM(1) TYPE C.

IF  'F110_FB21_FB1K_FB1D_FB05_FB08_FBZ1_FBZ2' CS SY-TCODE .
  DATA: WL_BEWAR TYPE BSEG-BEWAR,
        XBSEG_AUX LIKE LINE OF XBSEG,
        W_ZFIT0030 LIKE LINE OF T_ZFIT0030,
        V_TABIX TYPE SY-TABIX.

  CLEAR FLAGM.
  LOOP AT XBSEG INTO XBSEG_AUX WHERE HKONT IS NOT INITIAL.
    V_TABIX = SY-TABIX.
    IF FLAGM IS INITIAL.
        SELECT SINGLE *
        INTO  W_ZFIT0030
        FROM ZFIT0030
        WHERE HKONT  = XBSEG_AUX-HKONT
        AND   COND IN ('CV','C').
        IF SY-SUBRC = 0.
            FLAGM = 'E'.
            WL_BEWAR = W_ZFIT0030-BEWAR.
            IF W_ZFIT0030-COND = 'CV' OR W_ZFIT0030-COND = 'C'.
               FLAGM = 'X'.
               EXIT.
            ENDIF.
        ENDIF.
    ENDIF.
    IF ( FLAGM = 'E' AND XBSEG_AUX-BEWAR IS INITIAL ) .
       XBSEG_AUX-BEWAR = WL_BEWAR.
       MODIFY XBSEG INDEX V_TABIX FROM XBSEG_AUX.
    ENDIF.
  ENDLOOP.
  IF FLAGM = 'X'.
     LOOP AT XBSEG INTO XBSEG_AUX WHERE HKONT IS NOT INITIAL.
        V_TABIX = SY-TABIX.
        XBSEG_AUX-BEWAR = WL_BEWAR.
       MODIFY XBSEG INDEX V_TABIX FROM XBSEG_AUX.
     ENDLOOP.
 ENDIF.

  CLEAR FLAGM.
ENDIF.
if 'FB01_FBB1_FBS1_FB1S_FB08_FB50_FBR2_FB05_FBVB_FBV1' cs sy-tcode   and
    sy-ucomm = 'BU'.

    call function 'G_SET_GET_ALL_VALUES'
      exporting
        class         = '0000'
        setnr         = 'CONTAS_EC-CS'
      tables
        set_values    = t_hkont
      exceptions
        set_not_found = 1
        others        = 2.
      if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

    loop at xbseg.

      read table t_hkont with key from = xbseg-hkont.

      if sy-subrc eq 0.

        if xbseg-vbund is initial.

          message e000(z01) with 'Sociedade Parceira Obrigatória para esta conta.'.

          endif.

      endif.

    endloop.
  endif.

  "copiado da Z_ENHANCEMENT_02 em 16.07.2013
  IF ( 'FBB1_FBS1_FB50_F.81_FBR2_FB01_FBR2' CS SY-TCODE ) AND SY-UCOMM = 'BU'.
*** 07/12/12 ALRS
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS         = '0000'
          SETNR         = 'MAGGI_CTAS_TPMV_BPC'
        TABLES
          SET_VALUES    = T_HKONT
        EXCEPTIONS
          SET_NOT_FOUND = 1
          OTHERS        = 2.
        IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
     LOOP AT XBSEG.

          READ TABLE T_HKONT WITH KEY FROM = XBSEG-HKONT.

          IF SY-SUBRC EQ 0.

            IF XBSEG-BEWAR IS INITIAL.

              MESSAGE E000(Z01) WITH 'Tipo de Movimento de Consolidação '
                                      'obrigatório para esta conta.'.
            ELSE.
              SELECT *
               INTO TABLE T_ZFIT0030
               FROM ZFIT0030
               WHERE HKONT  = XBSEG-HKONT.
               IF T_ZFIT0030[] IS NOT INITIAL.
                  CLEAR: MSGE, FLAGM.
                  FLAGM = 'N'.
                  LOOP AT T_ZFIT0030.
                     IF T_ZFIT0030-BEWAR EQ XBSEG-BEWAR.
                        FLAGM = 'S'.
                     ENDIF.
                  ENDLOOP.
                  IF FLAGM = 'N'.
                      LOOP AT T_ZFIT0030.
                         CONCATENATE MSGE T_ZFIT0030-BEWAR INTO MSGE SEPARATED BY SPACE.
                      ENDLOOP.
                      CONCATENATE 'Tp mvto correto:' MSGE INTO MSGE SEPARATED BY SPACE.
                      MESSAGE E000(Z01) WITH MSGE.
                  ENDIF.
               ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.
  ENDIF.

  IF ( 'FBZ1_FBZ2' CS SY-TCODE ) AND SY-UCOMM = 'BU'.
*** 24/04/13 ALRS
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          CLASS         = '0000'
          SETNR         = 'MAGGI_EMPRESA_EXTERIOR'
        TABLES
          SET_VALUES    = T_BUKRS
        EXCEPTIONS
          SET_NOT_FOUND = 1
          OTHERS        = 2.
        IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
     SORT T_BUKRS BY FROM.
     LOOP AT XBSEG.

          READ TABLE T_BUKRS WITH KEY FROM = XBSEG-BUKRS.

          IF SY-SUBRC EQ 0.
            SELECT *
               INTO TABLE T_ZFIT0030
               FROM ZFIT0030
               WHERE HKONT  = XBSEG-HKONT.
            IF T_ZFIT0030[] IS NOT INITIAL.
               IF XBSEG-BEWAR IS INITIAL.

                 MESSAGE E000(Z01) WITH 'Tipo de Movimento  '
                                         'obrigatório para esta conta.'.
               ELSE.
                CLEAR: MSGE, FLAGM.
                FLAGM = 'N'.
                LOOP AT T_ZFIT0030.
                   IF T_ZFIT0030-BEWAR EQ XBSEG-BEWAR.
                      FLAGM = 'S'.
                   ENDIF.
                ENDLOOP.
                IF FLAGM = 'N'.
                    LOOP AT T_ZFIT0030.
                       CONCATENATE MSGE T_ZFIT0030-BEWAR INTO MSGE SEPARATED BY SPACE.
                    ENDLOOP.
                    CONCATENATE 'Tp mvto correto:' MSGE INTO MSGE SEPARATED BY SPACE.
                    MESSAGE E000(Z01) WITH MSGE.
                ENDIF.

               ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.
  ENDIF.

ENDENHANCEMENT.
