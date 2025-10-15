FUNCTION ZSD_DEFINE_CERT_MOV.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_TP_MOV) TYPE  CHAR01
*"     REFERENCE(I_DT_MOVIMENTO) TYPE  ZDT_MOV OPTIONAL
*"     REFERENCE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_QTDE_MOV) TYPE  J_1BNETQTY OPTIONAL
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_KVGR3) TYPE  KVGR3 OPTIONAL
*"     REFERENCE(I_WA_ZSDT0125) TYPE  ZSDT0125 OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_CD_CERTIFICACAO) TYPE  ZCD_CERT
*"     REFERENCE(E_QTDE_CERT) TYPE  J_1BNETQTY
*"     REFERENCE(E_MSG_CERTIFICACAO) TYPE  ZMSG_CERT
*"----------------------------------------------------------------------

  DATA: TG_ZSDT0122      TYPE TABLE OF ZSDT0122   WITH HEADER LINE,
        TG_ZSDT0123      TYPE TABLE OF ZSDT0123   WITH HEADER LINE,
        TG_ZSDT0125      TYPE TABLE OF ZSDT0125   WITH HEADER LINE,
        TG_ZSDT0125_GRP  TYPE TABLE OF ZSDT0125   WITH HEADER LINE.

  DATA: WA_ZSDT0125  TYPE ZSDT0125.

  DATA: VL_MOV_CERT        TYPE ZSDT0125-QTDE_CERT,
        VL_SLD_CERT        TYPE ZSDT0125-QTDE_CERT,
        VL_SLD_MOV         TYPE ZSDT0125-QTDE_CERT,
        VL_TOT_ENT         TYPE ZSDT0125-QTDE_CERT,
        VL_TOT_SAI         TYPE ZSDT0125-QTDE_CERT.

  CHECK I_TP_MOV IS NOT INITIAL.
  CHECK I_DT_MOVIMENTO IS NOT INITIAL.
  CHECK I_MATNR IS NOT INITIAL.
  CHECK I_QTDE_MOV > 0.

  CLEAR: E_CD_CERTIFICACAO , E_QTDE_CERT.

  "Check Movimentos Entradas Estornados
  SELECT *
    FROM ZSDT0125 AS A INTO TABLE TG_ZSDT0125
   WHERE MATNR EQ I_MATNR
     AND LOEKZ = ''
     AND EXISTS ( SELECT B~BELNR
                    FROM RBKP AS B
                   WHERE B~BELNR EQ A~FT_BELNR
                     AND B~GJAHR EQ A~FT_GJAHR
                     AND B~STBLG NE '' ).

  LOOP AT TG_ZSDT0125.
    TG_ZSDT0125-LOEKZ = 'X'.
    MODIFY ZSDT0125 FROM TG_ZSDT0125.
  ENDLOOP.

  CASE I_TP_MOV.
    WHEN 'E'. "Entradas

      CHECK I_LIFNR IS NOT INITIAL.

      CLEAR: WA_ZSDT0125.

      WA_ZSDT0125 = I_WA_ZSDT0125.

      "Verificar se existe parâmetro que define 100% dos movimentos como certificados para Safra/Fornecedor/Material.
      CLEAR: TG_ZSDT0123.
      SELECT SINGLE *
        FROM ZSDT0123 INTO TG_ZSDT0123
       WHERE LIFNR           EQ I_LIFNR
         AND MATNR           EQ I_MATNR
         AND DT_INI          <= I_DT_MOVIMENTO
         AND DT_FIM          >= I_DT_MOVIMENTO
         AND CD_CERTIFICACAO NE ''
         AND CERT_TOT        NE ''. "100% Movimentação Certificada

      IF ( SY-SUBRC = 0 ) AND ( TG_ZSDT0123-CD_CERTIFICACAO IS NOT INITIAL ) .
        E_CD_CERTIFICACAO = TG_ZSDT0123-CD_CERTIFICACAO.
        RETURN.
      ENDIF.

      "Procurar parâmetro de Certificação que tenha saldo para atribuir na movimentação.
      SELECT *
        FROM ZSDT0123 INTO TABLE TG_ZSDT0123
       WHERE LIFNR    EQ I_LIFNR
         AND MATNR    EQ I_MATNR
         AND DT_INI   <= I_DT_MOVIMENTO
         AND DT_FIM   >= I_DT_MOVIMENTO
         AND MENGE    > 0
         AND DT_INI   NE '00000000'
         AND DT_FIM   NE '00000000'
         AND CERT_TOT EQ ''
         AND CD_CERTIFICACAO NE ''
      ORDER BY CD_CERTIFICACAO.

      CHECK TG_ZSDT0123[] IS NOT INITIAL.

      VL_SLD_MOV = I_QTDE_MOV.

      LOOP AT TG_ZSDT0123.

        CLEAR: VL_MOV_CERT, VL_SLD_CERT.

        "Buscar a movimentação do Certificado para o Safra/Fornecedor/Material.
        CLEAR: VL_MOV_CERT.
        SELECT SINGLE SUM( QTDE_CERT )
          FROM ZSDT0125 INTO VL_MOV_CERT
         WHERE LIFNR           = I_LIFNR
           AND MATNR           = I_MATNR
           AND CD_CERTIFICACAO = TG_ZSDT0123-CD_CERTIFICACAO
           AND DT_MOVIMENTO   >= TG_ZSDT0123-DT_INI
           AND DT_MOVIMENTO   <= TG_ZSDT0123-DT_FIM
           AND LOEKZ           = ''
           AND DEVOLVIDA       = ''.

        "Calcula Saldo com Quantidade Parametrizada Certificado x Quantidade Movimentação Certificado.
        VL_SLD_CERT = TG_ZSDT0123-MENGE - VL_MOV_CERT.

        IF ( VL_SLD_CERT <= 0 ).
          CONTINUE.
        ENDIF.

        IF VL_SLD_CERT < VL_SLD_MOV.

          WA_ZSDT0125-CD_CERTIFICACAO = TG_ZSDT0123-CD_CERTIFICACAO.
          WA_ZSDT0125-QTDE_CERT       = VL_SLD_CERT.

          MODIFY ZSDT0125 FROM WA_ZSDT0125.

          SUBTRACT VL_SLD_CERT FROM VL_SLD_MOV.
          CONTINUE.
        ELSE.
          WA_ZSDT0125-CD_CERTIFICACAO = TG_ZSDT0123-CD_CERTIFICACAO.
          WA_ZSDT0125-QTDE_CERT       = VL_SLD_MOV.

          MODIFY ZSDT0125 FROM WA_ZSDT0125.

          RETURN.
        ENDIF.

      ENDLOOP.

      "Caso sobrar saldo do movimento, atribuir sem certificação
      IF VL_SLD_MOV > 0.
        CLEAR: WA_ZSDT0125-CD_CERTIFICACAO.
        WA_ZSDT0125-QTDE_CERT = VL_SLD_MOV.

        MODIFY ZSDT0125 FROM WA_ZSDT0125.
      ENDIF.


    WHEN 'S'. "Saídas

      CHECK I_WERKS IS NOT INITIAL.

      E_QTDE_CERT = I_QTDE_MOV.

      "Verificar Movimentação para o Centro/Material
      CLEAR: TG_ZSDT0125_GRP.
      SELECT WERKS MATNR CD_CERTIFICACAO
        FROM ZSDT0125 INTO CORRESPONDING FIELDS OF TABLE TG_ZSDT0125_GRP
       WHERE WERKS           EQ I_WERKS
         AND MATNR           EQ I_MATNR
         AND LOEKZ           EQ ''
         AND DEVOLVIDA       EQ ''
         AND CD_CERTIFICACAO NE ''
      GROUP BY WERKS MATNR CD_CERTIFICACAO.

      SORT TG_ZSDT0125_GRP BY WERKS MATNR CD_CERTIFICACAO.
      DELETE ADJACENT DUPLICATES FROM TG_ZSDT0125_GRP COMPARING WERKS MATNR CD_CERTIFICACAO.

      CHECK TG_ZSDT0125_GRP[] IS NOT INITIAL.

      SORT TG_ZSDT0125_GRP BY CD_CERTIFICACAO.

      "Verifica se tem entradas e Saldo do produto certificado no centro da baixa.
      LOOP AT TG_ZSDT0125_GRP.

        CLEAR: VL_TOT_ENT.

        "Busca Total de Entradas para o Centro/Material/Fornecedor
        CLEAR: VL_TOT_ENT.
        SELECT SINGLE SUM( QTDE_CERT )
          FROM ZSDT0125 INTO VL_TOT_ENT
         WHERE WERKS           EQ TG_ZSDT0125_GRP-WERKS
           AND MATNR           EQ TG_ZSDT0125_GRP-MATNR
           AND CD_CERTIFICACAO EQ TG_ZSDT0125_GRP-CD_CERTIFICACAO
           AND LOEKZ           EQ ''
           AND DEVOLVIDA       EQ ''.

        CHECK VL_TOT_ENT > 0.

        "Busca Total de Saídas para o Centro/Material
        CLEAR: VL_TOT_SAI.
        SELECT SINGLE SUM( QTDE_MOV )
          FROM ZSDT0126 INTO VL_TOT_SAI
         WHERE WERKS           EQ TG_ZSDT0125_GRP-WERKS
           AND MATNR           EQ TG_ZSDT0125_GRP-MATNR
           AND CD_CERTIFICACAO EQ TG_ZSDT0125_GRP-CD_CERTIFICACAO
           AND LOEKZ           EQ ''.

        "Caso tenha saldo do Material no Centro
        IF ( VL_TOT_ENT  - VL_TOT_SAI ) >= I_QTDE_MOV.
          E_CD_CERTIFICACAO = TG_ZSDT0125_GRP-CD_CERTIFICACAO.

          "Busca Mensagem
          SELECT SINGLE *
            FROM ZSDT0122 INTO TG_ZSDT0122
           WHERE WERKS           = I_WERKS
             AND MATNR           = I_MATNR
             AND KVGR3           = I_KVGR3
             AND CD_CERTIFICACAO = TG_ZSDT0125_GRP-CD_CERTIFICACAO
             AND DT_INI         <= I_DT_MOVIMENTO
             AND DT_FIM         >= I_DT_MOVIMENTO.

          IF ( SY-SUBRC = 0 ) AND ( TG_ZSDT0122-MSG IS NOT INITIAL ).
            E_MSG_CERTIFICACAO = TG_ZSDT0122-MSG.
          ENDIF.

          RETURN.
        ENDIF.

      ENDLOOP.

  ENDCASE.


ENDFUNCTION.
