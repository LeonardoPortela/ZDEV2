class ZCL_IM_BADI_SCD_SAVE definition
  public
  final
  create public .

*"* public components of class ZCL_IM_BADI_SCD_SAVE
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_BADI_SCD_SAVE .
protected section.
*"* protected components of class ZCL_IM_BADI_SCD_SAVE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_BADI_SCD_SAVE
*"* do not include other source files here!!!

  methods CK_VALORES_CONDICOES
    importing
      !I_SCD type V54A0_SCDD
    exceptions
      ERRO .
ENDCLASS.



CLASS ZCL_IM_BADI_SCD_SAVE IMPLEMENTATION.


  METHOD ck_valores_condicoes.

    "Verificar Valor de Frete
    "REBEL
    DATA: wa_zlest0164 TYPE zlest0164,
          it_zlest0164 TYPE TABLE OF zlest0164.

    LOOP AT i_scd-x-item INTO DATA(wa_item) WHERE vfkp-netwr IS NOT INITIAL.
      DATA(it_condicoes) = wa_item-komv[].
      SORT it_condicoes BY kappl kschl.
      DELETE ADJACENT DUPLICATES FROM it_condicoes COMPARING kappl kschl.
      SELECT SINGLE * INTO @DATA(wa_vttk) FROM vttk WHERE tknum EQ @wa_item-vfkp-rebel.
      IF sy-subrc IS INITIAL.
        LOOP AT it_condicoes INTO DATA(wa_condicoes).

          CLEAR: wa_zlest0164.
          wa_zlest0164-kappl = wa_condicoes-kappl.
          wa_zlest0164-kschl = wa_condicoes-kschl.
          wa_zlest0164-vsart = wa_vttk-vsart.
          wa_zlest0164-tplst = wa_vttk-tplst.
          wa_zlest0164-tdlnr = wa_vttk-tdlnr.
          wa_zlest0164-kwert = wa_condicoes-kwert.

          READ TABLE it_zlest0164 ASSIGNING FIELD-SYMBOL(<fs_zlest0164>)
          WITH KEY kappl = wa_zlest0164-kappl
                   kschl = wa_zlest0164-kschl
                   vsart = wa_zlest0164-vsart
                   tplst = wa_zlest0164-tplst
                   tdlnr = wa_zlest0164-tdlnr.
          IF sy-subrc IS INITIAL.
            ADD wa_zlest0164-kwert TO <fs_zlest0164>-kwert.
          ELSE.
            APPEND wa_zlest0164 TO it_zlest0164.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDLOOP.

    SELECT * INTO TABLE @DATA(it_validar)
      FROM zlest0164.

    LOOP AT it_zlest0164 INTO wa_zlest0164.

      "Tipo de condição/Tipo de expedição/Local de organização de transportes/Nº do agente de frete
      READ TABLE it_validar INTO DATA(wa_validar)
      WITH KEY kappl = wa_zlest0164-kappl
               kschl = wa_zlest0164-kschl
               vsart = wa_zlest0164-vsart
               tplst = wa_zlest0164-tplst
               tdlnr = wa_zlest0164-tdlnr.
      "Se encontrou Tipo de expedição/Local de organização de transportes/Nº do agente de frete
      "E Valor da Condição Maior que Valor Limite
      IF sy-subrc IS INITIAL AND wa_zlest0164-kwert GT wa_validar-kwert.
        DATA(lc_001) = 'Tipo de expedição: ' && wa_zlest0164-vsart && '/ Org.Transp.: ' && wa_zlest0164-tplst && '/ Ag.Frete: ' && wa_zlest0164-tdlnr.
        "===================================================USER STORY 61743 / Anderson Oenning
        IF wa_validar-kschl EQ 'ZPED'.
          MESSAGE e152(zles) WITH wa_zlest0164-kschl lc_001 RAISING erro.
        ELSE.
          MESSAGE e137(zles) WITH wa_zlest0164-kschl lc_001 RAISING erro.
        ENDIF.
        "===================================================USER STORY 61743 / Anderson Oenning
      ELSEIF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      "Tipo de condição/Tipo de expedição/Local de organização de transportes
      READ TABLE it_validar INTO wa_validar
      WITH KEY kappl = wa_zlest0164-kappl
               kschl = wa_zlest0164-kschl
               vsart = wa_zlest0164-vsart
               tplst = wa_zlest0164-tplst
               tdlnr = space.
      "Se encontrou Tipo de expedição/Local de organização de transportes
      "E Valor da Condição Maior que Valor Limite
      IF sy-subrc IS INITIAL AND wa_zlest0164-kwert GT wa_validar-kwert.
        DATA(lc_002) = 'Tipo de expedição: ' && wa_zlest0164-vsart && '/ Org.Transp.: ' && wa_zlest0164-tplst.
        "===================================================USER STORY 61743 / Anderson Oenning
        IF wa_validar-kschl EQ 'ZPED'.
          MESSAGE e152(zles) WITH wa_zlest0164-kschl lc_002 RAISING erro.
        ELSE.
          MESSAGE e137(zles) WITH wa_zlest0164-kschl lc_002 RAISING erro.
        ENDIF.
        "===================================================USER STORY 61743 / Anderson Oenning
      ELSEIF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      "Tipo de condição/Tipo de expedição
      READ TABLE it_validar INTO wa_validar
      WITH KEY kappl = wa_zlest0164-kappl
               kschl = wa_zlest0164-kschl
               vsart = wa_zlest0164-vsart
               tplst = space
               tdlnr = space.
      "Se encontrou Tipo de expedição
      "E Valor da Condição Maior que Valor Limite
      IF sy-subrc IS INITIAL AND wa_zlest0164-kwert GT wa_validar-kwert.
        DATA(lc_003) = 'Tipo de expedição: ' && wa_zlest0164-vsart.
        "===================================================USER STORY 61743 / Anderson Oenning
        IF wa_validar-kschl EQ 'ZPED'.
          MESSAGE e152(zles) WITH wa_zlest0164-kschl lc_003 RAISING erro.
        ELSE.
          MESSAGE e137(zles) WITH wa_zlest0164-kschl lc_003 RAISING erro.
        ENDIF.
        "===================================================USER STORY 61743 / Anderson Oenning
      ELSEIF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      "Tipo de condição
      READ TABLE it_validar INTO wa_validar
      WITH KEY kappl = wa_zlest0164-kappl
               kschl = wa_zlest0164-kschl
               vsart = space
               tplst = space
               tdlnr = space.
      "Se encontrou Tipo de expedição
      "E Valor da Condição Maior que Valor Limite
      IF sy-subrc IS INITIAL AND wa_zlest0164-kwert GT wa_validar-kwert.
        DATA(lc_004) = 'Tipo de condição: ' && wa_zlest0164-kschl.
        "===================================================USER STORY 61743 / Anderson Oenning
        IF wa_validar-kschl EQ 'ZPED'.
          MESSAGE e152(zles) WITH wa_zlest0164-kschl lc_004 RAISING erro.
        ELSE.
          MESSAGE e137(zles) WITH wa_zlest0164-kschl lc_004 RAISING erro.
        ENDIF.

        "===================================================USER STORY 61743 / Anderson Oenning
      ELSEIF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD IF_EX_BADI_SCD_SAVE~AT_SAVE.

  TYPES :
    BEGIN OF Y_LFBW,
      LIFNR     TYPE LFBW-LIFNR,
      WITHT     TYPE LFBW-WITHT,
      WT_WITHCD TYPE LFBW-WT_WITHCD,
    END OF Y_LFBW.


  DATA :
    VL_LIFNR    TYPE LFA1-LIFNR,
    VL_DATE     TYPE SY-DATUM,
    VL_ACUM     TYPE ZLEST0023-INSSACUM,
    VL_ACUMB    TYPE ZLEST0023-IRRF,
    VL_ACUMA    TYPE ZLEST0023-INSSACUM,
    VL_INSS     TYPE ZLEST0023-INSS,
    WK_KOMV     TYPE KOMV,
    VL_BUKRS    TYPE TTDS-BUKRS,
    VL_DTAFIM   TYPE SY-DATUM,
    VL_DTAINC   TYPE SY-DATUM,
    VL_WTMAX    TYPE T059MINMAX-WT_WTMAX,
    VL_LFBW     TYPE Y_LFBW,
    WK_OBJ      TYPE V54A0_REFOBJ,
    WK_VTTK     TYPE VTTK,
    WK_SCD      TYPE V54A0_SCDD,
    WK_X        TYPE V54A0_SCD,
    WK_Y        TYPE V54A0_SCD,
    WK_ITEM     TYPE V54A0_SCD_ITEM_TAB,
    WK_ITEM_AUX TYPE V54A0_SCD_ITEM,
    WK_VFKP     TYPE V54A0_VFKP,
    WK_VFKK     TYPE V54A0_VFKK,
    WK_0023     TYPE ZLEST0023,
    WK_0023A    TYPE ZLEST0023,
    WK_KOMVX    TYPE KOMV,
    WK_KOMVA    TYPE TABLE OF KOMV,
    VL_BASEIRRF TYPE ZLEST0023-BASEIRRF,
    VL_IRRF     TYPE ZLEST0023-IRRF,
    TI_0023     TYPE TABLE OF ZLEST0023,
    WK_0032     TYPE ZLEST0032.

  CLEAR : VL_LIFNR,     VL_ACUM, VL_INSS, WK_KOMV,     VL_DTAFIM, VL_DTAINC,  VL_WTMAX, VL_LFBW,  WK_OBJ,  VL_BUKRS,WK_KOMVA, WK_KOMVX, VL_ACUMB,
          WK_VTTK,      WK_SCD,  WK_X,    WK_ITEM_AUX, WK_VFKP,   WK_0023,    WK_0023A, VL_ACUMA, WK_VFKK, VL_DATE, VL_BASEIRRF, VL_IRRF,  WK_0032.

  READ TABLE I_REFOBJ_TAB INTO WK_OBJ INDEX 1.
  WK_VTTK = WK_OBJ-VTTKF.

  READ TABLE I_SCD_TAB INTO WK_SCD INDEX 1.
  WK_X     = WK_SCD-X.
  WK_ITEM  = WK_X-ITEM.
  WK_Y     = WK_SCD-Y.
  WK_VFKK  = WK_Y-VFKK.

* Controle de transferencia e estorno
  READ TABLE WK_ITEM INTO WK_ITEM_AUX INDEX 1.
  WK_VFKP  = WK_ITEM_AUX-VFKP.

* Controle de transferencia e estorno
  READ TABLE WK_ITEM INTO WK_ITEM_AUX INDEX 1.
  WK_KOMVA = WK_ITEM_AUX-KOMV[].

  IF WK_VFKK-UPDKZ = 'D'.

    DELETE FROM ZLEST0023 WHERE TKNUM = WK_VTTK-TKNUM.
    DELETE FROM ZLEST0032 WHERE TKNUM = WK_VTTK-TKNUM.

  ELSE.

*    LOOP AT I_SCD_TAB INTO DATA(WA_SCD_TAB).
*
*      ME->CK_VALORES_CONDICOES(
*        EXPORTING
*          I_SCD = WA_SCD_TAB
*        EXCEPTIONS
*         ERRO   = 1
*         OTHERS = 2 ).
*
*      IF SY-SUBRC IS NOT INITIAL.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING NO_SAVE.
*      ENDIF.
*
*    ENDLOOP.

    CASE WK_VFKP-SLSTOR.

      WHEN 'X'.

      WHEN OTHERS.

        MOVE :
        SY-MANDT               TO WK_0032-MANDT,
        WK_VFKP-REBEL          TO WK_0032-TKNUM,
        WK_VFKP-FKNUM          TO WK_0032-FKNUM,
        WK_VFKP-EBELN          TO WK_0032-EBELN,
        WK_VFKP-EBELP          TO WK_0032-EBELP,
        WK_VFKP-LBLNI          TO WK_0032-LBLNI,
        WK_VTTK-ADD03          TO WK_0032-ADD03,
        SY-DATUM               TO WK_0032-DATA,
        SY-UZEIT               TO WK_0032-HORA,
        SY-UNAME               TO WK_0032-NOME.
        MODIFY ZLEST0032 FROM WK_0032.

* Proprietário do veículo - Subcontrado
        SELECT SINGLE LIFNR
        INTO   VL_LIFNR
        FROM   VTPA
        WHERE  VBELN = WK_VTTK-TKNUM               AND
               POSNR = '000000'                    AND
               PARVW = 'PV'.

        IF SY-SUBRC IS INITIAL.

* Determinação da empresa
          SELECT SINGLE BUKRS
          INTO   VL_BUKRS
          FROM   TTDS
          WHERE  TPLST = WK_VTTK-TPLST.

* Proprietário do veículo
          SELECT SINGLE LIFNR WITHT WT_WITHCD
          INTO   VL_LFBW
          FROM   LFBW
          WHERE  LIFNR  = VL_LIFNR      AND
                 WITHT  IN ('IN', 'IJ').

          IF SY-SUBRC IS INITIAL.

* Montantes mínimos/máximos por código IRF
            SELECT WT_WTMAX WT_DATE
            INTO  (VL_WTMAX, VL_DATE)
            FROM  T059MINMAX UP TO 1 ROWS
            WHERE LAND1     = 'BR'                 AND
                  WITHT     = VL_LFBW-WITHT        AND
                  WT_WITHCD = VL_LFBW-WT_WITHCD
            ORDER BY WT_DATE DESCENDING.
            ENDSELECT.

            IF SY-SUBRC IS INITIAL.

* Acumulado do INSS
              SELECT *
              INTO TABLE    TI_0023
              FROM          ZLEST0023
              WHERE         BUKRS = VL_BUKRS              AND
                            LIFNR = VL_LIFNR              AND
                            DATAB <= SY-DATUM             AND
                            DATBI >= SY-DATUM.

              IF NOT SY-SUBRC IS INITIAL.

                CHECK NOT WK_VFKP-KZWI4 IS INITIAL.

* Determinação da data final do registro
                CALL FUNCTION 'LAST_DAY_OF_MONTHS'
                  EXPORTING
                    DAY_IN            = SY-DATUM
                  IMPORTING
                    LAST_DAY_OF_MONTH = VL_DTAFIM
                  EXCEPTIONS
                    DAY_IN_NO_DATE    = 1
                    OTHERS            = 2.

* Determinação da data inicio do registro
                CONCATENATE SY-DATUM(4)  SY-DATUM+4(2)  '01' INTO  VL_DTAINC.

* Obtenção dos valores de INSS
                VL_INSS = - ( WK_VFKP-KZWI4 ).
                VL_ACUM = - ( WK_VFKP-KZWI4 ).

* Validação do valor de teto da retenção
                IF VL_INSS > VL_WTMAX.
                  VL_INSS =  0.
                ENDIF.

* Validação do valor de teto da retenção
                IF VL_ACUM > VL_WTMAX.
                  VL_ACUM = VL_WTMAX.
                ENDIF.

                READ TABLE WK_KOMVA INTO WK_KOMVX WITH KEY KSCHL = 'ZBIR'.

                IF SY-SUBRC IS INITIAL.
                  VL_BASEIRRF = WK_KOMVX-KWERT.
                ENDIF.

                READ TABLE WK_KOMVA INTO WK_KOMVX WITH KEY KSCHL = 'ZIRF'.

                IF SY-SUBRC IS INITIAL.
                  VL_IRRF     = - ( WK_KOMVX-KWERT ).
                ENDIF.

                MOVE :
                SY-MANDT               TO WK_0023-MANDT,
                VL_BUKRS               TO WK_0023-BUKRS,
                VL_LIFNR               TO WK_0023-LIFNR,
                VL_DTAINC              TO WK_0023-DATAB,
                VL_DTAFIM              TO WK_0023-DATBI,
                WK_VTTK-TKNUM          TO WK_0023-TKNUM,
                VL_ACUM                TO WK_0023-INSSACUM,
                VL_INSS                TO WK_0023-INSS,
                VL_BASEIRRF            TO WK_0023-BASEIRRF,
                VL_IRRF                TO WK_0023-IRRF,
                SY-DATUM               TO WK_0023-ERDAT,
                SY-UZEIT               TO WK_0023-UZEIT,
                SY-UNAME               TO WK_0023-UNAME.

                MODIFY ZLEST0023 FROM WK_0023.

              ELSE.

* Valor do documento
* Acumulado do INSS
                SELECT SINGLE *
                INTO          WK_0023A
                FROM          ZLEST0023
                WHERE         BUKRS = VL_BUKRS              AND
                              LIFNR = VL_LIFNR              AND
                              DATAB <= SY-DATUM             AND
                              DATBI >= SY-DATUM             AND
                              TKNUM =  WK_VTTK-TKNUM.

                IF SY-SUBRC IS INITIAL.

* Valor Acumulado
                  LOOP AT TI_0023 INTO WK_0023.
                    VL_ACUM     = VL_ACUM     + WK_0023-INSS.
                    VL_BASEIRRF = VL_BASEIRRF + WK_0023-BASEIRRF.
                    VL_ACUMB    = VL_ACUMB    + WK_0023-IRRF.
                    AT LAST.
                      VL_ACUM     = VL_ACUM     - WK_0023A-INSS.
                      VL_BASEIRRF = VL_BASEIRRF - WK_0023A-BASEIRRF.
                    ENDAT.
                  ENDLOOP.

* Valor do INSS
                  VL_INSS        = WK_0023A-INSS.
                  VL_ACUM        = VL_ACUM + VL_INSS.
                  VL_IRRF        = - ( WK_VFKP-KZWI5 ).

                  MOVE :
                  VL_ACUM                TO WK_0023A-INSSACUM,
                  VL_INSS                TO WK_0023A-INSS,
                  VL_BASEIRRF            TO WK_0023-BASEIRRF,
                  VL_IRRF                TO WK_0023-IRRF,
                  SY-DATUM               TO WK_0023A-ERDAT,
                  SY-UZEIT               TO WK_0023A-UZEIT,
                  SY-UNAME               TO WK_0023A-UNAME.

                  MODIFY ZLEST0023 FROM WK_0023A.

                ELSE.

* Determinação da data final do registro
                  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
                    EXPORTING
                      DAY_IN            = SY-DATUM
                    IMPORTING
                      LAST_DAY_OF_MONTH = VL_DTAFIM
                    EXCEPTIONS
                      DAY_IN_NO_DATE    = 1
                      OTHERS            = 2.

* Determinação da data inicio do registro
                  CONCATENATE SY-DATUM(4)  SY-DATUM+4(2)  '01' INTO  VL_DTAINC.


                  READ TABLE WK_KOMVA INTO WK_KOMVX WITH KEY KSCHL = 'ZBIR'.

                  IF SY-SUBRC IS INITIAL.
                    VL_BASEIRRF = ( WK_KOMVX-KAWRT * WK_KOMVX-KBETR ) / 1000.
                  ENDIF.

* Valor Acumulado
                  LOOP AT TI_0023 INTO WK_0023.
                    VL_ACUM     = VL_ACUM     + WK_0023-INSS.
                    AT LAST.
                      VL_ACUM     = VL_ACUM     - WK_0023A-INSS.
                    ENDAT.
                  ENDLOOP.

* Valor do INSS e acumulado
                  VL_INSS        = - ( WK_VFKP-KZWI4 ).
                  VL_IRRF        = - ( WK_VFKP-KZWI5 ).
                  VL_ACUMA       = VL_ACUM  + VL_INSS.

                  IF VL_ACUMA >= VL_WTMAX.
                    VL_INSS = VL_WTMAX - VL_ACUM.
                    VL_ACUM = VL_WTMAX.
                  ELSE.
                    VL_ACUM = VL_ACUMA.
                  ENDIF.

                  MOVE :
                  SY-MANDT               TO WK_0023-MANDT,
                  VL_BUKRS               TO WK_0023-BUKRS,
                  VL_LIFNR               TO WK_0023-LIFNR,
                  VL_DTAINC              TO WK_0023-DATAB,
                  VL_DTAFIM              TO WK_0023-DATBI,
                  WK_VTTK-TKNUM          TO WK_0023-TKNUM,
                  VL_ACUM                TO WK_0023-INSSACUM,
                  VL_INSS                TO WK_0023-INSS,
                  VL_BASEIRRF            TO WK_0023-BASEIRRF,
                  VL_IRRF                TO WK_0023-IRRF,
                  SY-DATUM               TO WK_0023-ERDAT,
                  SY-UZEIT               TO WK_0023-UZEIT,
                  SY-UNAME               TO WK_0023-UNAME.

                  MODIFY ZLEST0023 FROM WK_0023.

                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

    ENDCASE.

  ENDIF.

ENDMETHOD.


method IF_EX_BADI_SCD_SAVE~BEFORE_UPDATE.
endmethod.


METHOD if_ex_badi_scd_save~check_complete.

  DATA: vl_vbeln TYPE vbak-vbeln,
        "TL_ITEM  TYPE V54A0_SCD_ITEM_TAB,
        "SL_ITEM  LIKE LINE OF TL_ITEM,
        "SL_VFKP  TYPE V54A0_VFKP,
        vl_msg   TYPE char100,
        vl_add03 TYPE vttk-add03,
        vl_belnr TYPE rbkp-belnr.

  "CHECK SY-TCODE EQ 'VI02'.

  "TL_ITEM[] = I_SCD-X-ITEM[].
  "READ TABLE TL_ITEM INTO SL_ITEM INDEX 1.
  "SL_VFKP = SL_ITEM-VFKP.

  me->ck_valores_condicoes(
    EXPORTING
      i_scd = i_scd
    EXCEPTIONS
     erro  =  1
     OTHERS = 2 ).

  IF sy-subrc IS NOT INITIAL.
    IF sy-batch EQ abap_false.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING error.
  ENDIF.

  "Resgata Informação que está no banco para verificar antes de gravar/deletar
  SELECT *
    INTO TABLE @DATA(it_vfkp)
    FROM vfkp
   WHERE fknum EQ @i_scd-fknum
     AND netwr NE 0       "Valor líquido em moeda do item custos de frete
     AND ebeln NE @space  "Nº do documento de compras
     AND lblni NE @space. "Nº folha registro de serviços

  "CHECK NOT SL_VFKP-POSTX IS INITIAL AND SL_VFKP-SLSTOR EQ 'X'.
  CHECK sy-subrc IS INITIAL.

  LOOP AT it_vfkp INTO DATA(wa_vfkp).

    "MIRO
    SELECT SINGLE rbkp~belnr INTO vl_belnr
      FROM rseg
     INNER JOIN rbkp ON rbkp~belnr = rseg~belnr AND rbkp~gjahr = rseg~gjahr AND rbkp~stblg = ''
     WHERE rseg~ebeln EQ wa_vfkp-ebeln
       AND rseg~lfbnr EQ wa_vfkp-lblni.

    IF sy-subrc IS INITIAL.
      MESSAGE i836(sd) WITH 'Custo de Frete possui MIRO referenciada:' vl_belnr INTO vl_msg.
      RAISE error.
    ENDIF.

    "Ordem de Venda
    SELECT SINGLE add03 INTO vl_add03
       FROM vttk
      WHERE tknum EQ wa_vfkp-rebel.

    IF vl_add03 = '0000000001'. "Frete proprio

      SELECT SINGLE vbeln INTO vl_vbeln
        FROM vbak
       WHERE tknum EQ wa_vfkp-rebel.
      "WHERE TKNUM EQ SL_VFKP-POSTX. POSTX também está gravado o rebel só que é texto e o usuário pode digitar o que quiser

      " 126780 - VI02 - Verificar se OV de serviço de frete foi estornada - PANF - inicio
      SELECT vbeln
        INTO TABLE @DATA(lt_fat)
        FROM vbfa
        WHERE vbelv = @vl_vbeln
          AND vbtyp_n = 'M'.

      IF sy-subrc = 0.

        SELECT COUNT(*)
            FROM vbfa
            FOR ALL ENTRIES IN @lt_fat
            WHERE vbelv = @lt_fat-vbeln
               AND vbeln <> @space.

        IF sy-subrc = 0.

          DATA: wl_orderheaderin  TYPE bapisdh1,
                wl_orderheaderinx TYPE bapisdh1x,
                wl_bape_vbak      TYPE bape_vbak,
                wl_bape_vbakx     TYPE bape_vbakx,
                tl_bapiparex      TYPE TABLE OF bapiparex,
                sl_bapiparex      TYPE bapiparex,
                t_return_vt       TYPE TABLE OF bapiret2.

*--------------------------------------
*-- monta bapi
*--------------------------------------
          FREE: wl_orderheaderin,wl_orderheaderinx,
                tl_bapiparex.

          wl_bape_vbak-vbeln           = vl_vbeln.
          wl_bape_vbak-tknum           = ''.
          sl_bapiparex-structure       = 'BAPE_VBAK'.
          sl_bapiparex-valuepart1      = wl_bape_vbak.
          APPEND sl_bapiparex         TO tl_bapiparex.

          CLEAR sl_bapiparex.
          wl_bape_vbakx-vbeln          = vl_vbeln.
          wl_bape_vbakx-tknum          = 'X'.
          sl_bapiparex-structure       = 'BAPE_VBAKX'.
          sl_bapiparex-valuepart1      = wl_bape_vbakx.
          APPEND sl_bapiparex         TO tl_bapiparex.

          wl_orderheaderin-bill_block  = '10'.
          wl_orderheaderinx-updateflag = 'U'.
          wl_orderheaderinx-bill_block = 'X'.

          CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              salesdocument    = vl_vbeln
              order_header_in  = wl_orderheaderin
              order_header_inx = wl_orderheaderinx
            TABLES
              return           = t_return_vt
              extensionin      = tl_bapiparex.

        ELSE.

          MESSAGE i836(sd) WITH 'Custo de Frete possui ordem de venda referenciada:' vl_vbeln INTO vl_msg.
          RAISE error.

        ENDIF.

      ENDIF.

**      IF sy-subrc IS INITIAL.
**        MESSAGE i836(sd) WITH 'Custo de Frete possui ordem de venda referenciada:' vl_vbeln INTO vl_msg.
**        RAISE error.
**      ENDIF.
      " 126780 - VI02 - Verificar se OV de serviço de frete foi estornada - PANF - fim
    ENDIF.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
