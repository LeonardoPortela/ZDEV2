*&---------------------------------------------------------------------*
*&  Include  ZXM06U42
*&---------------------------------------------------------------------*
i_ekko-ztipcontra = i_ekko-ztipcontra.
*I_EKKO-ZDESTI = I_EKKO-ZDESTI.
DATA i_erro(1).

IF i_ucomm = 'BTN_TRE'.

*  READ TABLE tekkn INTO DATA(wekkn) WITH KEY ebelp = i_ekpo-ebelp.
*  CALL FUNCTION 'Z_MM_TREINA_COMPRAS'
*    EXPORTING
*      i_lifnr = i_ekko-lifnr
*      i_ebeln = i_ekko-ebeln
*      i_ebelp = i_ekpo-ebelp
*      i_kostl = wekkn-kostl
*      i_saknr = wekkn-sakto
*      i_matnr = i_ekpo-matnr
*      i_btn   = 'X'
*      i_tipo  = 'P'
*    IMPORTING
*      i_erro  = i_erro.
ELSEIF i_ucomm = 'MELEAV' OR i_ucomm = 'MEBACK' OR i_ucomm = 'MECANC'.
  DELETE FROM zmmt0105
          WHERE ebeln   = ' '
          AND   ebelp   = i_ekpo-ebelp
          AND   lifnr   = i_ekko-lifnr
          AND   tipo    = 'P'.
ENDIF.

*----------------------------------------------------------------------*
* Descrição  : Bloqueio para Compras com RECAP                         *
*----------------------------------------------------------------------*
IF i_ucomm = 'MESAVE' AND ekpo_ci-recap IS INITIAL
   AND ( sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N' ).

  IF i_ekpo-knttp = 'A'.

    "Bloqueio RECAP
    CALL FUNCTION 'ZFMM_BLOQUEIO_RECAP'
      EXPORTING
        i_ekko   = i_ekko
        i_ekpo   = i_ekpo
      IMPORTING
        e_zrecap = ekpo_ci-recap
      TABLES
        tekkn    = tekkn.

  ENDIF.

ENDIF.


IF i_ucomm = 'REC' AND ( sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N' ).

  IF i_ekpo-knttp = 'A'.

    "Bloqueio RECAP
    CALL FUNCTION 'ZFMM_BLOQUEIO_RECAP'
      EXPORTING
        i_ekko   = i_ekko
        i_ekpo   = i_ekpo
      IMPORTING
        e_zrecap = ekpo_ci-recap
      TABLES
        tekkn    = tekkn.

  ENDIF.

ENDIF.
