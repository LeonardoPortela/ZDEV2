"Name: \PR:SAPMF05A\FO:OPEN_FI_PERFORM_1140_E_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 Z_ENHANCEMENT_F_02.
 DATA: t_hkont     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
       t_bukrs     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
       t_uskd      TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
       t_blart     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
       t_zfit0030  TYPE TABLE OF zfit0030 WITH HEADER LINE,
       it_zglt034  TYPE TABLE OF zglt034,
       wa_zglt034  TYPE  zglt034,
       msge(50)    TYPE c,
       flagm(1)    TYPE c,
       e_status(1),
       e_messa(64).

 DATA: tl_parametros TYPE ustyp_t_parameters,
       wl_parametros TYPE ustyp_parameters.


 IF xbkpf-bukrs IS NOT INITIAL AND xbkpf-budat IS NOT INITIAL.
   CALL FUNCTION 'Z_CONTROLE_FECHAMES'
     EXPORTING
       i_bukrs  = xbkpf-bukrs
       i_data   = xbkpf-budat
     IMPORTING
       e_status = e_status
       e_messa  = e_messa
     EXCEPTIONS
       error    = 1
       OTHERS   = 2.

   IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   IF  e_status = 'E'.
     MESSAGE e000(z01) WITH e_messa.
   ENDIF.
 ENDIF.

 "FBB1
 IF  sy-tcode = 'FBB1' AND sy-calld NE 'X'.
   CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
     EXPORTING
       user_name           = sy-uname
     TABLES
       user_parameters     = tl_parametros
     EXCEPTIONS
       user_name_not_exist = 1
       OTHERS              = 2.
   IF sy-subrc <> 0.
   ENDIF.

   READ TABLE tl_parametros INTO wl_parametros
    WITH KEY parid = 'ZFBB1'.

   IF sy-subrc NE 0.
     MESSAGE e000(z01) WITH 'Seu usuário (PARAMETRO ZFBB1)'
                            ' não tem acesso a esta transação!'.
   ENDIF.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       class         = '0000'
       setnr         = 'MAGGI_FBB1_BLART'
     TABLES
       set_values    = t_blart
     EXCEPTIONS
       set_not_found = 1
       OTHERS        = 2.
   IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   DATA(_ok) = 'N'.
   READ TABLE t_blart WITH KEY from = xbkpf-blart.
   IF sy-subrc = 0.
     _ok = 'S'.
   ENDIF.
   IF xbkpf-blart = 'VA' .
     _ok = 'S'.
   ENDIF.
   IF _ok = 'N' OR
      xbkpf-ldgrp NE '50'.
     MESSAGE e000(z01) WITH 'Somente permitido lançamentos tipo VA '
                            'ou SET MAGGI_FBB1_BLART  e ledger 50!'.
   ENDIF.

 ENDIF.

 IF 'FBR2_FB01_FBB1' CS sy-tcode OR ( 'FB05_FB1K' CS sy-tcode  AND sy-calld IS INITIAL ).
   DATA:         w_tbsl     TYPE tbsl.
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       class         = '0000'
       setnr         = 'MAGGI_F-02-BLOQ_KD'
     TABLES
       set_values    = t_uskd
     EXCEPTIONS
       set_not_found = 1
       OTHERS        = 2.
   IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   LOOP AT xbseg.
*          select SINGLE *
*            from tbsl
*            into w_tbsl
*            where bschl = XBSEG-bschl.

     IF xbseg-koart = 'K' AND xbseg-shkzg = 'H' AND xbseg-xauto IS INITIAL.
       READ TABLE t_uskd WITH KEY from = sy-uname.
       IF sy-subrc NE 0.
         IF sy-langu = 'P'.
           MESSAGE e000(z01) WITH 'Lançamento nas chaves de fornecedores'
                                  ' somente pode ser registrado pela '
                                  'transação ZGL0016'.
         ELSEIF sy-langu = 'S'.
           MESSAGE e000(z01) WITH 'Publicado en los proveedores  '
                                   ' claves sólo pueden ser registrados por la ZGL0016 transacción.'.
         ELSE.
           MESSAGE e000(z01) WITH 'Posted on suppliers '
                                  'only keys can be registered by the transaction ZGL0016.'.

         ENDIF.

       ENDIF.
     ENDIF.
   ENDLOOP.
 ENDIF.

 IF  'F110_FB21_FB1K_FB1D_FB05_FB08_FBZ1_FBZ2' CS sy-tcode .
   DATA: wl_bewar   TYPE bseg-bewar,
         xbseg_aux  LIKE LINE OF xbseg,
         w_zfit0030 LIKE LINE OF t_zfit0030,
         v_tabix    TYPE sy-tabix.

   CLEAR flagm.
   LOOP AT xbseg INTO xbseg_aux WHERE hkont IS NOT INITIAL.
     v_tabix = sy-tabix.
     IF flagm IS INITIAL.
       SELECT SINGLE *
       INTO  w_zfit0030
       FROM zfit0030
       WHERE hkont  = xbseg_aux-hkont
       AND   cond IN ('CV','C').
       IF sy-subrc = 0.
         flagm = 'E'.
         wl_bewar = w_zfit0030-bewar.
         IF w_zfit0030-cond = 'CV' OR w_zfit0030-cond = 'C'.
           flagm = 'X'.
           EXIT.
         ENDIF.
       ENDIF.
     ENDIF.
     IF ( flagm = 'E' AND xbseg_aux-bewar IS INITIAL ) .
       xbseg_aux-bewar = wl_bewar.
       MODIFY xbseg INDEX v_tabix FROM xbseg_aux.
     ENDIF.
   ENDLOOP.
   IF flagm = 'X'.
     LOOP AT xbseg INTO xbseg_aux WHERE hkont IS NOT INITIAL.
       v_tabix = sy-tabix.
       xbseg_aux-bewar = wl_bewar.
       MODIFY xbseg INDEX v_tabix FROM xbseg_aux.
     ENDLOOP.
   ENDIF.

   CLEAR flagm.
 ENDIF.
* break abap.
 "IF SY-TCODE = 'FB01' AND
 IF 'FB01_FBB1_FBS1_FB1S_FB08_FB50_FBR2_FB05_FBVB_FBV1_FBB1' CS sy-tcode   AND   xbkpf-bukrs NE '0201' AND
    sy-ucomm = 'BU'.

*     IF SY-TCODE = 'FBB1'.
*
*        LOOP AT XBKPF.
*
*          WA_ZGLT034-LOTE         = XBKPF-BELNR.
*          WA_ZGLT034-DESCR_LOTE   = XBKPF-BKTXT.
*          WA_ZGLT034-BUKRS        = XBKPF-BUKRS.
*          WA_ZGLT034-USNAM        = XBKPF-USNAM.
*          WA_ZGLT034-DEP_RESP     = '83'.
*          WA_ZGLT034-STATUS_LOTE  = 'L'.
*          WA_ZGLT034-DATA_ATUAL   = XBKPF-CPUDT.
*          WA_ZGLT034-HORA_ATUAL   = XBKPF-CPUTM.
*          WA_ZGLT034-USUARIO      = XBKPF-USNAM.
*          WA_ZGLT034-TCODE        = XBKPF-TCODE.
*
*             LOOP AT XBSEG.
*               IF XBSEG-SHKZG = 'H'.
*                WA_ZGLT034-VALOR      = XBSEG-WRBTR . "(SHKZG = ‘H’ )
*              ENDIF.
*             ENDLOOP.
*
*          APPEND WA_ZGLT034 TO IT_ZGLT034.
*
*          CLEAR WA_ZGLT034.
*
*        ENDLOOP.
*
*        MODIFY ZGLT034 FROM TABLE IT_ZGLT034.
*     ENDIF.


   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       class         = '0000'
       setnr         = 'CONTAS_EC-CS'
     TABLES
       set_values    = t_hkont
     EXCEPTIONS
       set_not_found = 1
       OTHERS        = 2.
   IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

   "
   LOOP AT xbseg.
     READ TABLE t_hkont WITH KEY from = xbseg-hkont.

     IF sy-subrc EQ 0.

       IF xbseg-vbund IS INITIAL.

         MESSAGE e000(z01) WITH 'Sociedade Parceira Obrigatória para esta conta.'.

       ENDIF.

     ENDIF.

   ENDLOOP.

 ENDIF.

 IF ( 'FBB1_FBS1_FB50_F.81_FBR2_FB01_FBR2' CS sy-tcode ) AND sy-ucomm = 'BU'.
*** 07/12/12 ALRS
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       class         = '0000'
       setnr         = 'MAGGI_CTAS_TPMV_BPC'
     TABLES
       set_values    = t_hkont
     EXCEPTIONS
       set_not_found = 1
       OTHERS        = 2.
   IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   LOOP AT xbseg.

     READ TABLE t_hkont WITH KEY from = xbseg-hkont.

     IF sy-subrc EQ 0.

       IF xbseg-bewar IS INITIAL.

         MESSAGE e000(z01) WITH 'Tipo de Movimento de Consolidação '
                                 'obrigatório para esta conta.'.
       ELSE.
         SELECT *
          INTO TABLE t_zfit0030
          FROM zfit0030
          WHERE hkont  = xbseg-hkont.
         IF t_zfit0030[] IS NOT INITIAL.
           CLEAR: msge, flagm.
           flagm = 'N'.
           LOOP AT t_zfit0030.
             IF t_zfit0030-bewar EQ xbseg-bewar.
               flagm = 'S'.
             ENDIF.
           ENDLOOP.
           IF flagm = 'N'.
             LOOP AT t_zfit0030.
               CONCATENATE msge t_zfit0030-bewar INTO msge SEPARATED BY space.
             ENDLOOP.
             CONCATENATE 'Tp mvto correto:' msge INTO msge SEPARATED BY space.
             MESSAGE e000(z01) WITH msge.
           ENDIF.
         ENDIF.
       ENDIF.

     ENDIF.

   ENDLOOP.
 ENDIF.

 IF ( 'FBZ1_FBZ2' CS sy-tcode ) AND sy-ucomm = 'BU'.
*** 24/04/13 ALRS
   CALL FUNCTION 'G_SET_GET_ALL_VALUES'
     EXPORTING
       class         = '0000'
       setnr         = 'MAGGI_EMPRESA_EXTERIOR'
     TABLES
       set_values    = t_bukrs
     EXCEPTIONS
       set_not_found = 1
       OTHERS        = 2.
   IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
   SORT t_bukrs BY from.
   LOOP AT xbseg.

     READ TABLE t_bukrs WITH KEY from = xbseg-bukrs.

     IF sy-subrc EQ 0.
       SELECT *
          INTO TABLE t_zfit0030
          FROM zfit0030
          WHERE hkont  = xbseg-hkont.
       IF t_zfit0030[] IS NOT INITIAL.
         IF xbseg-bewar IS INITIAL.

           MESSAGE e000(z01) WITH 'Tipo de Movimento  '
                                   'obrigatório para esta conta.'.
         ELSE.
           CLEAR: msge, flagm.
           flagm = 'N'.
           LOOP AT t_zfit0030.
             IF t_zfit0030-bewar EQ xbseg-bewar.
               flagm = 'S'.
             ENDIF.
           ENDLOOP.
           IF flagm = 'N'.
             LOOP AT t_zfit0030.
               CONCATENATE msge t_zfit0030-bewar INTO msge SEPARATED BY space.
             ENDLOOP.
             CONCATENATE 'Tp mvto correto:' msge INTO msge SEPARATED BY space.
             MESSAGE e000(z01) WITH msge.
           ENDIF.

         ENDIF.
       ENDIF.

     ENDIF.

   ENDLOOP.
 ENDIF.

ENDENHANCEMENT.
