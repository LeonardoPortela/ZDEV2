*&---------------------------------------------------------------------*
*&  Include           ZXM06U44
*&---------------------------------------------------------------------*

DATA: V_DIV_CCUSTO LIKE CSKS-GSBER,
      V_DIV_CENTRO LIKE T130W-WERKS,
      V_WERKS1     LIKE EKPO-WERKS,
      V_WERKS2     LIKE EKPO-WERKS,
      V_EBELN      LIKE EKKO-EBELN,
      V_TABIX      LIKE SY-TABIX.

TYPES:
  BEGIN OF TY_EKPO,
    EBELP LIKE EKPO-EBELP,
    MATNR LIKE EKPO-MATNR,
    BUKRS LIKE EKPO-BUKRS,
    WERKS LIKE EKPO-WERKS,
  END OF   TY_EKPO.

DATA:
  IT_ZMMT0004     TYPE TABLE OF ZMMT0004,
  WA_ZMMT0004     LIKE ZMMT0004,
  WA_ZMMT0004_AUX LIKE ZMMT0004,
*  it_ekpo       type table of  XEKPO,
  WA_EKPO         LIKE  XEKPO,
  V_ATUALIZA(1)   TYPE C.

*"---------------------------------------------------------------------
*" Débito Posterior 20.10.2015 - ALRS
**"---------------------------------------------------------------------
*IF SY-TCODE EQ 'ME21N' OR
*   SY-TCODE EQ 'ME22N' OR
*   SY-TCODE EQ 'ME23N'.
*
*  CLEAR: WA_EKPO.
*  IF ( I_EKKO-BSART EQ 'ZDBP' ).
*    LOOP AT XEKPO INTO WA_EKPO.
*      IF WA_EKPO-LOEKZ NE ''.
*        CONTINUE.
*      ENDIF.
*      IF WA_EKPO-WEBRE = ' '.
*        MESSAGE E000(Z01) WITH 'Código revisão fatura '
*                               ' obrigatório para débito posterior'.
*      ENDIF.
*
*      "09.12.15 ALRS
*      IF WA_EKPO-WEUNB = ' '.
*        MESSAGE E000(Z01) WITH 'EM n/avaliada '
*                               ' obrigatório para débito posterior'.
*      ENDIF.
*
*      IF WA_EKPO-BEDNR IS INITIAL.
*        MESSAGE E000(Z01) WITH 'Informe o pedido referente '
*                               ' ao débito posterior'.
*      ELSE.
*        SELECT SINGLE EBELN
*          FROM EKKO
*          INTO V_EBELN
*          WHERE EBELN = WA_EKPO-BEDNR
*          AND   LOEKZ = ''.
*        IF SY-SUBRC NE 0.
*          MESSAGE E000(Z01) WITH 'Pedido referente '
*                                 ' ao débito posterior não existe'.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ELSEIF 'ZNB_ZOUT_REG_ZIM_ZPI' CS  I_EKKO-BSART.
*    LOOP AT XEKPO INTO WA_EKPO.
*      IF WA_EKPO-LOEKZ NE ''.
*        CONTINUE.
*      ENDIF.
*      IF WA_EKPO-WEBRE = ' '.
**        WA_EKPO-WEBRE = 'X'. "Não funciona aqui
**        MODIFY XEKPO FROM WA_EKPO  INDEX SY-TABIX TRANSPORTING WEBRE.
*        MESSAGE E000(Z01) WITH 'Código revisão fatura '
*                               ' obrigatório para este tipo de pedido'.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*ENDIF.
*"---------------------------------------------------------------------
*" Pesquisar se material e centro tem contrato. Atualização da
*" tabela ZMMT0004
*"---------------------------------------------------------------------
IF SY-TCODE EQ 'ME31K' OR
   SY-TCODE EQ 'ME32K' OR
   SY-TCODE EQ 'ZMM0081'.
  CLEAR: WA_EKPO,
         IT_ZMMT0004,
         WA_ZMMT0004,
         V_ATUALIZA.

  CASE SY-TCODE.
* PREENCHIMENTO DA TABELA ZMMT0004.
    WHEN 'ZMM0081'.
      LOOP AT XEKPO INTO WA_EKPO.
*          wa_zmmt0004-mandt = i_ekko-mandt.
        WA_ZMMT0004-MATNR = WA_EKPO-MATNR.
        WA_ZMMT0004-WERKS = WA_EKPO-WERKS.
        WA_ZMMT0004-BUKRS = I_EKKO-BUKRS.
        WA_ZMMT0004-EBELN = I_EKKO-EBELN.
        WA_ZMMT0004-EBELP = WA_EKPO-EBELP.
        WA_ZMMT0004-LIFNR = I_EKKO-LIFNR.
        WA_ZMMT0004-KDATB = I_EKKO-KDATB.
        WA_ZMMT0004-KDATE = I_EKKO-KDATE.
        WA_ZMMT0004-LOEKZ = SPACE.
* Início inclusão - Renato Sabbado - v.1.3
        WA_ZMMT0004-EKORG = I_EKKO-EKORG.
* Fim inclusão - Renato Sabbado - v.1.3
* Início inclusão - Renato Sabbado - v.1.4
        WA_ZMMT0004-PACKNO = WA_EKPO-PACKNO.
* Fim inclusão - Renato Sabbado - v.1.4
        MODIFY ZMMT0004 FROM WA_ZMMT0004.
      ENDLOOP.

    WHEN 'ME31K'.
      LOOP AT XEKPO INTO WA_EKPO.
*          wa_zmmt0004-mandt = i_ekko-mandt.
        WA_ZMMT0004-MATNR = WA_EKPO-MATNR.
        WA_ZMMT0004-WERKS = WA_EKPO-WERKS.
        WA_ZMMT0004-BUKRS = I_EKKO-BUKRS.
        WA_ZMMT0004-EBELN = I_EKKO-EBELN.
        WA_ZMMT0004-EBELP = WA_EKPO-EBELP.
        WA_ZMMT0004-LIFNR = I_EKKO-LIFNR.
        WA_ZMMT0004-KDATB = I_EKKO-KDATB.
        WA_ZMMT0004-KDATE = I_EKKO-KDATE.
        WA_ZMMT0004-LOEKZ = SPACE.
* Início inclusão - Renato Sabbado - v.1.3
        WA_ZMMT0004-EKORG = I_EKKO-EKORG.
* Fim inclusão - Renato Sabbado - v.1.3
* Início inclusão - Renato Sabbado - v.1.4
        WA_ZMMT0004-PACKNO = WA_EKPO-PACKNO.
* Fim inclusão - Renato Sabbado - v.1.4
        MODIFY ZMMT0004 FROM WA_ZMMT0004.
      ENDLOOP.


* ATUALIZAÇÃO DA TABELA ZMMT0004.
    WHEN 'ME32K'.

      SELECT *
        FROM ZMMT0004
        INTO TABLE IT_ZMMT0004
        WHERE EBELN EQ I_EKKO-EBELN.

      IF SY-SUBRC EQ 0.
        READ TABLE IT_ZMMT0004 INDEX 1 INTO WA_ZMMT0004.
*        LOOP AT it_zmmt0004 INTO wa_zmmt0004.
        CLEAR V_ATUALIZA.
* Alteração no campo de início do período de validade
        IF WA_ZMMT0004-KDATB NE I_EKKO-KDATB.
          WA_ZMMT0004-KDATB = I_EKKO-KDATB.
          V_ATUALIZA = 'X'.
        ENDIF.
* Alteração no campo de fim da validade
        IF WA_ZMMT0004-KDATE NE I_EKKO-KDATE.
          WA_ZMMT0004-KDATE = I_EKKO-KDATE.
          V_ATUALIZA = 'X'.
        ENDIF.
* Alteração no campo de código de eliminação no documento de compras
*          IF wa_zmmt0004-loekz NE i_ekko-loekz.
*            wa_zmmt0004-loekz = i_ekko-loekz.
*            v_atualiza = 'X'.
*          ENDIF.
*se ocorreu alguma modificação no cabeçalho, então a repassa a todos os registros
        IF NOT V_ATUALIZA IS INITIAL  .
          LOOP AT IT_ZMMT0004 INTO WA_ZMMT0004_AUX.
            MOVE: WA_ZMMT0004-KDATE TO WA_ZMMT0004_AUX-KDATE,
                  WA_ZMMT0004-LOEKZ TO WA_ZMMT0004_AUX-LOEKZ,
                  WA_ZMMT0004-KDATB TO WA_ZMMT0004_AUX-KDATB.
            MODIFY IT_ZMMT0004 FROM WA_ZMMT0004_AUX.
          ENDLOOP.
          MODIFY ZMMT0004 FROM TABLE IT_ZMMT0004.
        ENDIF.
        LOOP AT XEKPO INTO WA_EKPO.
          WA_ZMMT0004-MANDT   = I_EKKO-MANDT.
          WA_ZMMT0004-BUKRS   = WA_EKPO-BUKRS.
          WA_ZMMT0004-EBELN   = WA_EKPO-EBELN.
          WA_ZMMT0004-EBELP   = WA_EKPO-EBELP.
          WA_ZMMT0004-MATNR   = WA_EKPO-MATNR.
          WA_ZMMT0004-LIFNR   = I_EKKO-LIFNR.
          WA_ZMMT0004-WERKS   = WA_EKPO-WERKS.
          WA_ZMMT0004-KDATB   = I_EKKO-KDATB.
          WA_ZMMT0004-KDATE   = I_EKKO-KDATE.
          WA_ZMMT0004-LOEKZ   = WA_EKPO-LOEKZ.

          MODIFY ZMMT0004 FROM WA_ZMMT0004.
        ENDLOOP.
      ENDIF.
  ENDCASE.
*  endif.

ENDIF.
*
*break abap.
*
*IF i_ekko-inco1 <> 'CIF' AND
*   i_ekko-inco1 <> 'FOB' AND
*   i_ekko-inco1 <> 'FCA'.
*
*  MESSAGE e000(su) WITH 'Incoterms informado errado tem que ser'
*                         ' CIF, FCA ou FOB'.
*
*ENDIF.
*
*IF i_ekko-inco1 = 'FOB'.
*
*  READ TABLE xkomv WITH KEY kschl = 'FRT'.
*
*  IF sy-subrc <> 0.
*
*    MESSAGE e000(su) WITH 'Incluir a condição Frete(FRT).'.
*
*  ENDIF.
*
*  READ TABLE xkomv WITH KEY kschl = 'FRT'.
*
*  IF sy-subrc = 0.
*
*    IF xkomv-kbetr <= 0.
*
*      MESSAGE e000(su) WITH 'Valor do Montante para Frete,'
*      ' precisa ser maior que zero.'.
*
*    ENDIF.
*
*  ENDIF.
*
*ENDIF.
