"Name: \PR:SAPMV45A\FO:USEREXIT_MOVE_FIELD_TO_VBAP\SE:BEGIN\EI
ENHANCEMENT 0 Z_USEREXIT_MOVE_FIELD_TO_VBAP.

* -CSB-
* Chamado 129257 - 15.09.2014 - Ao entrar na tela carrega os dados
*verificar se teve alteções nos campos abaixo

      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- INICIO
      DATA:   BEGIN OF TG_VBAP OCCURS 125.
                INCLUDE STRUCTURE VBAPVB.
      DATA:   END OF TG_VBAP.

      DATA:   ST_VBAP        TYPE VBAPVB.

      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- FIM

      CASE SY-TCODE.

        WHEN: 'VA02'.

          IF  ( ( VBAK-AUART  EQ 'ZEXP' ) OR ( VBAK-AUART  EQ 'ZEXI' ) OR ( VBAK-AUART  EQ 'ZEXD' ) ).

            DATA: W_CAMPO(40),
                  IT_TABELA    TYPE TABLE OF VBAPVB,
                  WA_IT_TABELA TYPE  VBAPVB.


            FIELD-SYMBOLS: <FS_MATNR>   TYPE ANY,
                           <FS_KWMENG>  TYPE ANY,
                           <FS_WERKS>   TYPE ANY,
                           <FS_LGORT>   TYPE ANY,
                           <FS_CHARG>   TYPE ANY,
                           <FS_PARTNER> TYPE ANY,
                           <FS_VBAP>    TYPE VBAPVB.
            .

            ASSIGN ('(SAPMV45A)VBAP-MATNR')   TO <FS_MATNR>.  "(MATERIAL)
            ASSIGN ('(SAPMV45A)RV45A-KWMENG') TO <FS_KWMENG>. "(QUANTIDADE)
            ASSIGN ('(SAPMV45A)VBAP-WERKS')   TO <FS_WERKS>.  "(CENTRO)
            ASSIGN ('(SAPMV45A)VBAP-LGORT')   TO <FS_LGORT>.  "(DEPOSITO)
            ASSIGN ('(SAPMV45A)VBAP-CHARG')   TO <FS_CHARG>.  "(LOTE)
            ASSIGN ('(SAPLV09C)SDPARTNERLIST-PARTNER') TO <FS_PARTNER>. "(Parceiro)

            REFRESH: TG_VBAP.
            TG_VBAP[] = XVBAP[].
            CLEAR ST_VBAP.

            LOOP AT TG_VBAP ASSIGNING <FS_VBAP>.

              ST_VBAP = <FS_VBAP>.

              IF ST_VBAP-MATNR <> <FS_MATNR>.
                MESSAGE E899 WITH 'Material não pode ser alterado!'.
              ENDIF.

              IF ST_VBAP-KWMENG <> <FS_KWMENG>.
                MESSAGE E899 WITH 'Quantidade não pode ser alterada!'.
              ENDIF.

              IF ST_VBAP-WERKS  <> <FS_WERKS>.
                MESSAGE E899 WITH 'Centro não pode ser alterado!'.
              ENDIF.

              IF ST_VBAP-LGORT <> <FS_LGORT>.
                MESSAGE E899 WITH 'Depósito não pode ser alterado!'.
              ENDIF.

              IF ST_VBAP-CHARG <> <FS_CHARG>.
                MESSAGE E899 WITH 'Lote não pode ser alterado!'.
              ENDIF.


            ENDLOOP.
          ENDIF.

      ENDCASE.

      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- INICIO

      DATA: V_MTART      TYPE MTART,
            V_MSG        TYPE sTRING,
            V_TAMANHO    TYPE I,
            V_CONTADOR   TYPE I,
            VL_TXT01(50) TYPE C,
            VL_TXT02(50) TYPE C,
            VL_TXT03(50) TYPE C,
            VL_TXT04(50) TYPE C.

      REFRESH: TG_VBAP.
      TG_VBAP[] = XVBAP[].

      LOOP AT TG_VBAP INTO DATA(WA_VBAP).

        SELECT SINGLE MTART FROM MARA INTO V_MTART WHERE MATNR EQ WA_VBAP-MATNR.

        IF SY-SUBRC IS INITIAL.
          IF V_MTART EQ 'ZFER' AND WA_VBAP-SPART EQ '02'. "Produto acabado e Fertilizante

            CALL FUNCTION 'ZSD_VALIDACOES_OV'
              EXPORTING
                I_MATNR    = WA_VBAP-MATNR
                I_WERKS    = WA_VBAP-WERKS
              IMPORTING
                E_MENSAGEM = V_MSG.

            IF V_MSG IS NOT INITIAL.
              v_TAMANHO = STRLEN( V_MSG ).
              VL_TXT01 = V_MSG(50).
              VL_TXT02 = V_MSG+50(50).
              V_TAMANHO = V_TAMANHO - 100.
              IF V_TAMANHO > 50.
                VL_TXT03 = V_MSG+100(50).
                V_TAMANHO = V_TAMANHO - 50.
                VL_TXT04 = V_MSG+150(V_TAMANHO).
              ELSE.
                VL_TXT03 = V_MSG+100(V_TAMANHO).
              ENDIF.
              MESSAGE E836(SD) WITH VL_TXT01 VL_TXT02 VL_TXT03 VL_TXT04.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR:VL_TXT01, VL_TXT02, VL_TXT03, VL_TXT04.
      "    Incluir Validações ao criar OV de Fertilizantes - #169945 - BG  -- FIM


* Fim chamado 129257

*BBKO/Vagner Santos - Início da alteração - 03.10.2010
* Confrontar os valores dos impostos e leis da aba "País" do item do
* pedido com a tabela ZSDT0008.

* Tipos

      IF ( VBAK-VKORG NE '0100' ).

        TYPES: BEGIN OF Y_KNA1,
                 BRSCH TYPE KNA1-BRSCH,
                 REGIO TYPE KNA1-REGIO,
                 CITYC TYPE KNA1-CITYC,
                 KUNNR TYPE KNA1-KUNNR,
               END OF Y_KNA1,

               BEGIN OF Y_LFA1,
                 BRSCH TYPE LFA1-BRSCH,
                 REGIO TYPE LFA1-REGIO,
                 LIFNR TYPE LFA1-LIFNR,
               END OF Y_LFA1,

               BEGIN OF Y_T001W,
                 REGIO TYPE T001W-REGIO,
               END OF Y_T001W,

               BEGIN OF Y_MBEW,
                 MTORG TYPE MBEW-MTORG, "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
                 OWNPR TYPE MBEW-OWNPR,
               END OF Y_MBEW,

*-CS2025000025-#164218-27.01.2025-JT-inicio
               BEGIN OF Y_ZSDT0008,  "Y_ZSDT0370, "y_zsdt0008,
                 J_1BTXSDC  TYPE ZSDT0008-J_1BTXSDC,
                 J_1BTAXLW1 TYPE ZSDT0008-J_1BTAXLW1,
                 J_1BTAXLW2 TYPE ZSDT0008-J_1BTAXLW2,
                 J_1BTAXLW4 TYPE ZSDT0008-J_1BTAXLW4,
                 J_1BTAXLW5 TYPE ZSDT0008-J_1BTAXLW5,
               END OF Y_ZSDT0008.  "Y_ZSDT0370. "y_zsdt0008.
*-CS2025000025-#164218-27.01.2025-JT-fim

* Tabelas internas
        DATA: TI_XKOMV TYPE TABLE OF KOMV.

* Campos
        DATA: "VL_TXT01(50) TYPE C,
          "VL_TXT02(50) TYPE C,
          "VL_TXT03(50) TYPE C,
          "VL_TXT04(50) TYPE C,
          VL_TAXLAW TYPE J_1BTXIC3-TAXLAW,
          VL_SHTYP  TYPE ZSDT0008-SHTYP,  "ZSDT0370-SHTYP,  "zsdt0008-shtyp, "*-CS2025000025-#164218-27.01.2025-JT-inicio
          VL_TDLNR  TYPE ZSDT0008-TDLNR,  "ZSDT0370-TDLNR,  "zsdt0008-tdlnr, *-CS2025000025-#164218-27.01.2025-JT-inicio
          VL_LIFNR  TYPE VTPA-LIFNR,
          VL_KUNNR  TYPE VTPA-KUNNR.

*        DATA: LC_DADOS   TYPE ZSDE0183,   "*-CS2025000025-#164218-27.01.2025-JT-inicio
*              LC_RETORNO TYPE ZSDT0370_T. "*-CS2025000025-#164218-27.01.2025-JT-inicio

* Estruturas
        DATA: ST_KNA1     TYPE Y_KNA1,
              ST_LFA1     TYPE Y_LFA1,
              ST_T001W    TYPE Y_T001W,
              ST_MBEW     TYPE Y_MBEW,
              st_zsdt0008 TYPE y_zsdt0008,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*             ST_ZSDT0370 TYPE Y_ZSDT0370,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
              ST_XVBAP    TYPE VBAPVB,
              ST_XKOMV    TYPE KOMV,
              ST_VTTP     TYPE VTTP,
              ST_LIPS     TYPE LIPS,
              ST_VBAK_AUX TYPE VBAK,
              ST_VBFA_AUX TYPE VBFA,
              ST_T001K    TYPE T001K.

* Constantes
        CONSTANTS: C_IBRX    TYPE KOMV-KSCHL VALUE 'IBRX',
                   C_VA01    TYPE SY-TCODE   VALUE 'VA01',
                   C_VA02    TYPE SY-TCODE   VALUE 'VA02',
                   C_4470    TYPE SY-DYNNR   VALUE '4470',   "Tela de intes - Aba País
                   C_BR(2)   TYPE C          VALUE 'BR',
                   C_76(2)   TYPE C          VALUE '76',
                   C_ZTRO(4) TYPE C          VALUE 'ZTRO',
                   C_ZTRT(4) TYPE C          VALUE 'ZTRT',
                   C_ZTRH(4) TYPE C          VALUE 'ZTRH',
                   C_LR(2)   TYPE C          VALUE 'LR',
                   C_PC(2)   TYPE C          VALUE 'PC',
                   C_SD(2)   TYPE C          VALUE 'SD'.

* Somente para ordens tipo "ZTRO"
        IF VBAK-AUART = C_ZTRO OR VBAK-AUART = C_ZTRT OR VBAK-AUART = C_ZTRH.
          IF VBAK-AUART = C_ZTRO OR VBAK-AUART = C_ZTRT OR VBAK-AUART = C_ZTRH .
            CLEAR: VL_LIFNR, VL_KUNNR.
            SELECT KUNNR INTO VL_KUNNR
                         FROM VTPA
                      UP TO 1 ROWS
                        WHERE VBELN = VBAK-TKNUM
                          AND PARVW = C_LR.
            ENDSELECT.
            IF SY-SUBRC EQ 0.
* Obter informações do cliente parceiro LR
              SELECT SINGLE BRSCH REGIO CITYC KUNNR INTO ST_KNA1
                                                    FROM KNA1
                                                   WHERE KUNNR = VL_KUNNR.
            ENDIF.
            "
            SELECT LIFNR INTO VL_LIFNR
                 FROM VTPA
              UP TO 1 ROWS
                WHERE VBELN = VBAK-TKNUM
                  AND PARVW = C_PC.
            ENDSELECT.
            IF SY-SUBRC EQ 0.
* Obter informações do fornecedor PC
              SELECT SINGLE BRSCH REGIO LIFNR INTO ST_LFA1
                                                    FROM LFA1
                                                   WHERE LIFNR = VL_LIFNR.
            ENDIF.
          ELSE.
* Obter informações do cliente original
            SELECT SINGLE BRSCH REGIO CITYC KUNNR INTO ST_KNA1
                                                  FROM KNA1
                                                 WHERE KUNNR = VBAK-KUNNR.
            MOVE-CORRESPONDING ST_KNA1 TO ST_LFA1.
          ENDIF.
          CHECK SY-SUBRC EQ 0.

* Obter informações do centro
          SELECT SINGLE REGIO INTO ST_T001W
                              FROM T001W
                             WHERE WERKS = VBAP-WERKS.
          CHECK SY-SUBRC EQ 0.

* Obter informações do material.
          SELECT MTORG OWNPR INTO ST_MBEW
                       FROM MBEW
                  UP TO 1 ROWS
                      WHERE MATNR = VBAP-MATNR
                        AND BWKEY = VBAP-WERKS.
          ENDSELECT.
          CHECK SY-SUBRC EQ 0.

* Verificar se há alguma regra de excessão dinámica de ICMS
          CLEAR VL_TAXLAW.
          SELECT SINGLE TAXLAW INTO VL_TAXLAW
                               FROM J_1BTXIC3
                              WHERE LAND1    = C_BR
                                AND SHIPFROM = ST_T001W-REGIO
                                AND SHIPTO   = ST_KNA1-REGIO
                                AND GRUOP    = C_76
                                AND VALUE    = ST_KNA1-KUNNR
                                AND VALUE2   = VBAP-MATNR.

* Buscar Numero TKNUM quando...
          IF ( VBAK-AUART EQ 'ZCFH' ) AND
*      IF ( ( VBAK-AUART EQ 'ZCFH' ) OR ( VBAK-AUART EQ 'ZSFR' ) ) AND
              ( VBAK-VGBEL IS NOT INITIAL ) AND
              ( VBAK-TKNUM IS INITIAL     ).

            SELECT SINGLE *
              FROM VBFA INTO ST_VBFA_AUX
             WHERE VBELN   EQ VBAK-VGBEL
               AND VBTYP_N EQ 'M'
               AND VBTYP_V EQ 'C'.

            IF ( SY-SUBRC = 0 ) AND ( ST_VBFA_AUX-VBELV IS NOT INITIAL ).
              SELECT SINGLE *
                FROM VBAK INTO ST_VBAK_AUX
               WHERE VBELN EQ ST_VBFA_AUX-VBELV.

              IF ( SY-SUBRC = 0 ) AND ( ST_VBAK_AUX-TKNUM IS NOT INITIAL ).
                VBAK-TKNUM = ST_VBAK_AUX-TKNUM.
              ENDIF.
            ENDIF.
          ENDIF.


* O usuário já deverá ter informado o número de documento de transporte
* na aba Dados adicionais B
          CLEAR: VL_TDLNR, VL_SHTYP.
          SELECT SINGLE SHTYP TDLNR INTO (VL_SHTYP,VL_TDLNR)
                              FROM VTTK
                             WHERE TKNUM = VBAK-TKNUM.
* Pega Tomador do serviço frete
          CLEAR ST_T001K.
          SELECT SINGLE * INTO ST_VTTP
                        FROM VTTP
                        WHERE TKNUM = VBAK-TKNUM.
          IF SY-SUBRC = 0.
            SELECT SINGLE * INTO ST_LIPS
                          FROM LIPS
                          WHERE VBELN = ST_VTTP-VBELN.
            IF SY-SUBRC = 0.
              SELECT SINGLE * INTO ST_T001K
                            FROM T001K
                            WHERE BWKEY	=	ST_LIPS-WERKS.
            ENDIF.
          ENDIF.

********************************************************************** "163317 - CS2025000025 Melhoria ZSDT0011 - Determinação de impostos - PSA
          DATA: LR_MATNR TYPE RANGE OF MARA-MATNR,
                LR_MATKL TYPE RANGE OF MARA-MATKL,
                LR_EXTWG TYPE RANGE OF MARA-EXTWG,
                LR_BUKRS TYPE RANGE OF T001-BUKRS,
                LR_STEUC TYPE RANGE OF MARC-STEUC,
                LR_MTORG TYPE RANGE OF MBEW-MTORG,
                IT_MARA  TYPE MARA,
                IT_MARC  TYPE MARC,
                LV_KTOKD TYPE KNA1-KTOKD,
                LV_BUKRS TYPE J_1BBRANCH-BUKRS.

*      IF vbap-matnr IS NOT INITIAL.
*        SELECT SINGLE *
*          INTO it_mara
*          FROM mara
*         WHERE matnr = vbap-matnr.

*      lr_matkl[] = VALUE #( FOR p01 IN it_mara ( option = 'EQ' sign = 'I' low = p01-matkl ) ).
*      SORT lr_matkl.
*      DELETE ADJACENT DUPLICATES FROM lr_matkl.
*
*      lr_extwg[] = VALUE #( FOR p02 IN it_mara ( option = 'EQ' sign = 'I' low = p02-extwg ) ).
*      SORT lr_extwg.
*      DELETE ADJACENT DUPLICATES FROM lr_extwg.

*        SELECT SINGLE *
*          INTO it_marc
*          FROM marc
*         WHERE matnr = vbap-matnr
*           AND werks = vbap-werks.
*      ENDIF.

**********************************************************************

* O código do IVA é sempre "SD".
          ST_XKOMV-MWSK1 = C_SD.

* Selecionar os códigos dos direitos fiscais e leis na tabela local.
*-CS2025000025-#164218-27.01.2025-JT-inicio
      SELECT SINGLE j_1btxsdc j_1btaxlw1 j_1btaxlw2
                    j_1btaxlw4 j_1btaxlw5
                    INTO st_zsdt0008
                    FROM zsdt0008
                   WHERE auart      = vbak-auart
                     AND vkaus      = vbap-vkaus
                     "AND BRSCH      = ST_LFA1-BRSCH
                     AND uf_centro  = st_lfa1-regio
                     AND uf_cliente = st_kna1-regio
                     "AND CITYC      = ST_KNA1-CITYC
                     AND mwsk1      = st_xkomv-mwsk1
                     AND ownpr      = st_mbew-ownpr
                     AND shtyp      = vl_shtyp
                     AND tdlnr      = vl_tdlnr
                     AND bukrs      = st_t001k-bukrs.

*          LC_DADOS-AUART-VALOR      = VBAK-AUART.
*          LC_DADOS-VKAUS-VALOR      = VBAP-VKAUS.
*          LC_DADOS-MWSK1-VALOR      = ST_XKOMV-MWSK1.
*          LC_DADOS-UF_CENTRO-VALOR  = ST_LFA1-REGIO.
*          LC_DADOS-UF_CLIENTE-VALOR = ST_KNA1-REGIO.
*          LC_DADOS-OWNPR-VALOR      = ST_MBEW-OWNPR.
*          LC_DADOS-SHTYP-VALOR      = VL_SHTYP.
*          LC_DADOS-TDLNR-VALOR      = VL_TDLNR.
**         lc_dados-bukrs_toma-valor = st_t001k-bukrs.
*          LC_DADOS-BUKRS_EMIT-VALOR = ST_T001K-BUKRS.
*          LC_DADOS-KUNNR-VALOR      = VBAK-KUNNR.
*          LC_DADOS-WERKS-VALOR      = VBAP-WERKS.
*          LC_DADOS-MATNR-VALOR      = VBAP-MATNR.
*
*          LC_RETORNO = ZCL_IMPOSTOS=>GET_TAX_IMPOSTO( I_DADOS = LC_DADOS ).
*
*          READ TABLE LC_RETORNO INTO DATA(WC_RETORNO) INDEX 1.
*          IF SY-SUBRC = 0.
*            MOVE-CORRESPONDING WC_RETORNO  TO ST_ZSDT0370.
*          ENDIF.
*-CS2025000025-#164218-27.01.2025-JT-fim

          IF ( SY-SUBRC NE 0 ).
            VL_TXT01 = 'Parametrizar Leis Fiscais->'.
            CONCATENATE  VBAK-AUART '/'
                         VBAP-VKAUS '/_/'
                         ST_LFA1-REGIO '/'
                         ST_KNA1-REGIO '/_/'
                         ST_XKOMV-MWSK1 '/'
                         ST_MBEW-OWNPR '/'
                         VL_SHTYP '/'
                         VL_TDLNR '/'
                         ST_T001K-BUKRS  INTO VL_TXT02.

            VL_TXT03 = 'Procurar Departamento Fiscal tributário.'.
            MESSAGE E836(SD) WITH VL_TXT01 VL_TXT02 VL_TXT03 VL_TXT04.
          ELSEIF SY-SUBRC EQ 0.
            IF VBAP-J_1BTXSDC  NE ST_ZSDT0008-J_1BTXSDC. "ST_ZSDT0370-J_1BTXSDC.    "*-CS2025000025-#164218-27.01.2025-JT-inicio
              VBAP-J_1BTXSDC = ST_ZSDT0008-J_1BTXSDC.    "ST_ZSDT0370-J_1BTXSDC.       "*-CS2025000025-#164218-27.01.2025-JT-inicio
            ENDIF.
            IF ( VL_TAXLAW IS NOT INITIAL ) AND ( VBAK-AUART NE 'ZTER' ).
              VBAP-J_1BTAXLW1 = VL_TAXLAW.
            ELSEIF VBAP-J_1BTAXLW1 NE ST_ZSDT0008-J_1BTAXLW1. "ST_ZSDT0370-J_1BTAXLW1. "*-CS2025000025-#164218-27.01.2025-JT-inicio
              VBAP-J_1BTAXLW1 = ST_ZSDT0008-J_1BTAXLW1.       "ST_ZSDT0370-J_1BTAXLW1.       "*-CS2025000025-#164218-27.01.2025-JT-inicio
            ENDIF.
            IF VBAP-J_1BTAXLW2 NE ST_ZSDT0008-J_1BTAXLW2.     "ST_ZSDT0370-J_1BTAXLW2.     "*-CS2025000025-#164218-27.01.2025-JT-inicio
              VBAP-J_1BTAXLW2 = ST_ZSDT0008-J_1BTAXLW2.       "ST_ZSDT0370-J_1BTAXLW2.       "*-CS2025000025-#164218-27.01.2025-JT-inicio
            ENDIF.
            IF VBAP-J_1BTAXLW4 NE ST_ZSDT0008-J_1BTAXLW4.     "ST_ZSDT0370-J_1BTAXLW4.     "*-CS2025000025-#164218-27.01.2025-JT-inicio
              VBAP-J_1BTAXLW4 =  ST_ZSDT0008-J_1BTAXLW4.      "ST_ZSDT0370-J_1BTAXLW4.      "*-CS2025000025-#164218-27.01.2025-JT-inicio
            ENDIF.
            IF VBAP-J_1BTAXLW5 NE ST_ZSDT0008-J_1BTAXLW5.     "ST_ZSDT0370-J_1BTAXLW5.
              VBAP-J_1BTAXLW5 =  ST_ZSDT0008-J_1BTAXLW5.      "ST_ZSDT0370-J_1BTAXLW5.
            ENDIF.
          ENDIF.


        ENDIF.
*bbko/vagner santos - fim da alteração - 03.10.2010

*  VBAP-zzfield = xxxx-zzfield2.
        CHECK 1 = 1.
      ENDIF.

ENDENHANCEMENT.
