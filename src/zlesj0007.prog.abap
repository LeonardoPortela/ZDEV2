*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviario.
*& Data.....: 19.03.2014 14:35:22
*& Descrição: Acompanhamento de Descarga -
*& Este JOB tem como objetivo pegar todos os registros da ZLEST0080 que estão com o status;
*& L = Lido
*& E = ERRO
*& Para alimentar a ZLEST0039 quando o status for igual a L e eliminar o registro quando Status for igual a E (somente de 7 dias atras).
*&--------------------------------------------------------------------&*


REPORT  ZLESJ0007.

**********************************************************************
* TABELAS
**********************************************************************
TABLES: ZLEST0080, ZLEST0039.

DATA: GT_ZLEST0080 TYPE TABLE OF ZLEST0080,
      GW_ZLEST0080 TYPE ZLEST0080,
      GT_KNA1      TYPE TABLE OF KNA1,
      GW_KNA1      TYPE KNA1,
      GT_LFA1      TYPE TABLE OF LFA1,
      GW_LFA1      TYPE LFA1,
      VL_DATA      TYPE SY-DATUM.

"Selecionar todos os registros da ZLEST0080 onde o STATUS É L (lido) e E (erro).

SELECT * FROM ZLEST0080
  INTO TABLE GT_ZLEST0080
WHERE STATUS IN ('L','E').

CHECK NOT GT_ZLEST0080[] IS INITIAL.

SELECT * FROM LFA1
  INTO TABLE GT_LFA1
  FOR ALL ENTRIES IN GT_ZLEST0080
WHERE STCD1 EQ GT_ZLEST0080-CNPJ_DESTINO .

SELECT * FROM KNA1
  INTO TABLE GT_KNA1
  FOR ALL ENTRIES IN GT_ZLEST0080
 WHERE STCD1 EQ GT_ZLEST0080-CNPJ_DESCARGA.

IF ( SY-SUBRC EQ 0 ).

  LOOP AT GT_ZLEST0080 INTO GW_ZLEST0080.

    VL_DATA = SY-DATUM - 7.

    IF ( GW_ZLEST0080-DT_CADASTRO EQ VL_DATA ) AND ( GW_ZLEST0080-STATUS EQ 'E' ).

      DELETE
        FROM ZLEST0080
        WHERE CHAVE_NFE EQ GW_ZLEST0080-CHAVE_NFE
          AND ARQUIVO   EQ GW_ZLEST0080-ARQUIVO.

      COMMIT WORK.

      DELETE
        FROM ZLEST0082
         WHERE CHAVE_NFE EQ GW_ZLEST0080-CHAVE_NFE
           AND ARQUIVO   EQ GW_ZLEST0080-ARQUIVO.

      COMMIT WORK.

      CLEAR: GW_ZLEST0080, VL_DATA.
      CONTINUE.
    ELSE.

      READ TABLE GT_LFA1  INTO GW_LFA1 WITH KEY STCD1 = GW_ZLEST0080-CNPJ_DESTINO.
      IF ( SY-SUBRC EQ 0 ).
        READ TABLE GT_KNA1 INTO GW_KNA1 WITH KEY STCD1 = GW_ZLEST0080-CNPJ_DESCARGA.

        IF ( SY-SUBRC EQ 0 ).

          SELECT SINGLE * INTO @DATA(WA_ZLEST0039)
            FROM ZLEST0039
           WHERE PONTOTRANSB   EQ @GW_KNA1-KUNNR
             AND PONTOENTREGA  EQ @GW_LFA1-LIFNR
             AND CHAVE_NFE     EQ @GW_ZLEST0080-CHAVE_NFE.

          IF SY-SUBRC IS INITIAL.
            DATA(LW_ZLEST0039_AJUSTE) = WA_ZLEST0039.
            LW_ZLEST0039_AJUSTE-DATATRANSB = GW_ZLEST0080-DT_DESCARGA.
            LW_ZLEST0039_AJUSTE-PESOTRANSB = GW_ZLEST0080-PESO_DESCARGA.
            PERFORM VERIFICAR_CARGUERO IN PROGRAM ZLESI0005 USING WA_ZLEST0039 LW_ZLEST0039_AJUSTE IF FOUND.
            CLEAR: LW_ZLEST0039_AJUSTE.
          ENDIF.

          UPDATE ZLEST0039 SET DATATRANSB = GW_ZLEST0080-DT_DESCARGA
                               PESOTRANSB = GW_ZLEST0080-PESO_DESCARGA
                            WHERE PONTOTRANSB   EQ GW_KNA1-KUNNR
                              AND PONTOENTREGA  EQ GW_LFA1-LIFNR
                              AND CHAVE_NFE     EQ GW_ZLEST0080-CHAVE_NFE.


          IF ( SY-SUBRC EQ 0 ).
            COMMIT WORK.

            UPDATE ZLEST0080 SET STATUS = 'P'
                             WHERE CHAVE_NFE EQ GW_ZLEST0080-CHAVE_NFE
                               AND ARQUIVO   EQ GW_ZLEST0080-ARQUIVO.

            COMMIT WORK.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.
    CLEAR: GW_ZLEST0080, GW_LFA1, GW_KNA1.
  ENDLOOP.
ENDIF.
