class ZCL_DADOS_OV definition
  public
  final
  create public .

public section.

  class-methods I_VLR_REFERENCIA_OV
    importing
      !I_VBELN type VBELN_VF
      !I_VBELNN type VBELN_VF
    exporting
      !E_VLR_TOTAL type NETWR
      !E_VLR_PARCIAL type NETWR .
  class-methods I_VLR_OV
    importing
      !I_VBELN type VBELN_VF
    exporting
      !E_VLR_TOTAL type NETWR
      !E_VLR_PARCIAL type NETWR
      !E_LIFSP_12 type CHAR1 .
  class-methods I_VENC_REF_OV
    importing
      !T_VBAK type ZPME0052_T
      !DT_INIT type SY-DATUM
      !DT_FIM type SY-DATUM
    returning
      value(TL_VBRK) type ZPME0053_T .
  class-methods I_VLR_SALDO_FINANC
    importing
      !I_VBELN type VBELN_VF
    exporting
      !E_VLR_TOTAL type NETWR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DADOS_OV IMPLEMENTATION.


  method I_VENC_REF_OV.

    types:

      " Documento de vendas: dados de cabeçalho
      begin of TY_VBRK,
        VBELN type BKPF-AWKEY,
        FKART type VBRK-FKART,
        FKTYP type VBRK-FKTYP,
      end of TY_VBRK.

    data: W_SAIDA type ZPME0053.
    data: IT_VBRK type table of TY_VBRK.

    loop at T_VBAK assigning field-symbol(<S_VBAK>).
      "Selecionar faturas da ordem.
      select RK~VBELN RK~FKART RK~FKTYP
      from   VBRK  as RK
      inner join VBFA as FA  on FA~VBELN = RK~VBELN
      into table IT_VBRK
      where FA~VBELV = <S_VBAK>-VBELN
      and  FA~VBTYP_N  = 'M'
      and  FA~VBTYP_V  = 'C'
      and   STUFE      = '01' and RK~DRAFT = SPACE .

      if IT_VBRK is not initial.
        select *
          from BKPF
          into table @data(T_BKPF)
            for all entries in @IT_VBRK
            where AWKEY eq @IT_VBRK-VBELN.

        if IT_VBRK is not initial.
* ---> S4 Migration - 15/06/2023 - MA
*          SELECT *
*           FROM BSEG
*           INTO TABLE @DATA(T_BSEG)
*             FOR ALL ENTRIES IN @T_BKPF
*             WHERE BELNR EQ @T_BKPF-BELNR
*               AND BUKRS EQ @T_BKPF-BUKRS
*               AND GJAHR EQ @T_BKPF-GJAHR
*               AND FDTAG BETWEEN @DT_INIT AND @DT_FIM
*               AND BUZEI EQ '01'.

          data LT_FIELDS type FAGL_T_FIELD.
          data: LT_BSEG type table of BSEG,
                T_BSEG  type table of BSEG.

*          LT_FIELDS = value #( ( LINE = 'BUKRS' )
*                               ( LINE = 'BELNR' )
*                               ( LINE = 'GJAHR' )
*                               ).


          call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
            exporting
              IT_FOR_ALL_ENTRIES = T_BKPF
              I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
*             IT_FIELDLIST       = LT_FIELDS
            importing
              ET_BSEG            = LT_BSEG
            exceptions
              NOT_FOUND          = 1.

          delete LT_BSEG where FDTAG NOT BETWEEN DT_INIT and DT_FIM.
          delete LT_BSEG where BUZEI ne '01'.

          if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
            MOVE-CORRESPONDING LT_BSEG to T_BSEG.
            SY-DBCNT = LINES( LT_BSEG ).
          else.
            SY-SUBRC = 4.
            SY-DBCNT = 0.
          endif.
* <--- S4 Migration - 15/06/2023 - MA
        endif.


        if T_BSEG is not initial.
          loop at IT_VBRK into data(WA_VBRK) where VBELN eq <S_VBAK>-VBELN.
            read table T_BKPF into data(W_BKPF) with key AWKEY = WA_VBRK-VBELN.
            if SY-SUBRC eq 0.
              read table T_BSEG into data(W_BSEG) with key BELNR = W_BKPF-BELNR
                                                           BUKRS = W_BKPF-BUKRS
                                                           GJAHR = W_BKPF-GJAHR.

              if SY-SUBRC eq 0.
                W_SAIDA-VBELN = <S_VBAK>-VBELN.
                W_SAIDA-DOC_FATURA = WA_VBRK-VBELN.
                W_SAIDA-FDTAG = W_BSEG-FDTAG.
*
                append W_SAIDA to TL_VBRK.
                clear: W_SAIDA, W_BSEG, W_BKPF.
              endif.
            endif.
          endloop.
        endif.
      endif.
    endloop.
  endmethod.


  METHOD i_vlr_ov.

    DATA:
      vlr_multa TYPE zfit0026-vlr_multa_calc,
      vlr_juros TYPE zfit0026-vlr_juros_calc,
      vlr_mont  TYPE zfit0026-vlr_juros_calc,
      it_vbap   TYPE TABLE OF vbap,
      total_ov  TYPE vbap-netwr.


    CLEAR: e_vlr_total, e_vlr_parcial, vlr_mont, total_ov.

***  Buscar movimentações em relação recebimento.
    SELECT * FROM zfit0026 INTO TABLE @DATA(t_zfit0026) WHERE vbeln EQ @i_vbeln.



    IF t_zfit0026 IS NOT INITIAL.
      "Deletar quando o DOCNUM estiver vazio e ajuste estiver vazio.
      DELETE t_zfit0026 WHERE docnum EQ 0 AND ajuste EQ ' '.

      IF t_zfit0026 IS NOT INITIAL.
*** Totalizar o total de saida de juros e multa.
        LOOP AT t_zfit0026 ASSIGNING FIELD-SYMBOL(<w_zfit0026>).
          ADD <w_zfit0026>-mont_moeda TO vlr_mont.
        ENDLOOP.
      ENDIF.
    ENDIF.

****Selecionar informações da OV.
    SELECT *  FROM vbap INTO TABLE @DATA(t_vbap)
          WHERE vbeln EQ @i_vbeln
          AND NOT EXISTS ( SELECT *
                         FROM vbep
                        WHERE vbeln EQ vbap~vbeln
                          AND posnr EQ vbap~posnr
                          AND lifsp EQ '12' ).

    CLEAR total_ov.
    IF t_vbap IS NOT INITIAL.
      LOOP AT t_vbap INTO DATA(wa_vbap).
        ADD wa_vbap-netwr TO total_ov.
        ADD wa_vbap-mwsbp TO total_ov.
      ENDLOOP.
*** Inicio - Rubenilson Pereira - 02.09.25 #175147
    ELSE.
      e_lifsp_12 = abap_true.
    ENDIF.
*** Fim - Rubenilson Pereira - 02.09.25 #175147

    SELECT SINGLE *
    FROM vbak
    INTO @DATA(wa_vbak)
      WHERE vbeln EQ @i_vbeln.

**********************************************************************
** 151268 Verificar OV`s devolução - PANF - 28.08.24 - INICIO
    SELECT COUNT(*)
      FROM tvarvc
      WHERE name = 'ZSDT0060_TP_OV_DEVOLUCAO'
        AND low = wa_vbak-auart.
*   IF wa_vbak-auart EQ 'ZRPF'  OR wa_vbak-auart EQ 'ZROB'  OR wa_vbak-auart EQ 'ZREB'.
    IF sy-subrc = 0.
** 151268 Verificar OV`s devolução - PANF - 28.08.24 - Fim
********************************************************************
      total_ov  = total_ov  * -1.
    ELSEIF  wa_vbak-auart EQ 'ZREM' OR  wa_vbak-auart EQ 'ZRFU'.
      total_ov = 0.
    ENDIF.


    e_vlr_total = total_ov.
    e_vlr_parcial = ( total_ov - vlr_mont ).



  ENDMETHOD.


  METHOD I_VLR_REFERENCIA_OV.

    DATA:
      VLR_MULTA TYPE ZFIT0026-VLR_MULTA_CALC,
      VLR_JUROS TYPE ZFIT0026-VLR_JUROS_CALC,
      VLR_MONT  TYPE ZFIT0026-VLR_JUROS_CALC.

    CLEAR: E_VLR_TOTAL, E_VLR_PARCIAL, VLR_MONT.

***  Buscar movimentações em relação recebimento.
    SELECT * FROM ZFIT0026 INTO TABLE @DATA(T_ZFIT0026) WHERE VBELN EQ @I_VBELN  AND DOC_FATURA EQ @I_VBELNN.

*** Totalizar o total de saida de juros e multa.
    IF T_ZFIT0026 IS NOT INITIAL.
      LOOP AT T_ZFIT0026 ASSIGNING FIELD-SYMBOL(<W_ZFIT0026>).
        ADD <W_ZFIT0026>-MONT_MOEDA TO VLR_MONT.
      ENDLOOP.
    ENDIF.

****Selecionar informações da referencia.
    SELECT SINGLE *  FROM VBRK INTO @DATA(W_VBRK)
      WHERE VBELN EQ @I_VBELNN
      AND   FKSTO NE 'X'.


    E_VLR_TOTAL = ( W_VBRK-NETWR + W_VBRK-MWSBK ).
    E_VLR_PARCIAL = ( W_VBRK-NETWR + W_VBRK-MWSBK ) - VLR_MONT.
  ENDMETHOD.


  METHOD I_VLR_SALDO_FINANC.

    DATA:
      VLR_MULTA    TYPE ZFIT0026-VLR_MULTA_CALC,
      VLR_JUROS    TYPE ZFIT0026-VLR_JUROS_CALC,
      VLR_MONT     TYPE ZFIT0026-VLR_JUROS_CALC,
      VLR_SALD_FIN TYPE ZFIT0026-VLR_JUROS_CALC,
      IT_VBAP      TYPE TABLE OF VBAP,
      TOTAL_OV     TYPE VBAP-NETWR.


    CLEAR: E_VLR_TOTAL,  VLR_MONT.

***  Buscar movimentações em relação recebimento.
    SELECT * FROM ZFIT0026 INTO TABLE @DATA(T_ZFIT0026) WHERE VBELN EQ @I_VBELN.

*** Verificar lançamentos de multa, juros e desconto.
    IF T_ZFIT0026 IS NOT INITIAL.
      LOOP AT T_ZFIT0026 ASSIGNING FIELD-SYMBOL(<W_ZFIT0026>).
        CLEAR: VLR_SALD_FIN.
        VLR_SALD_FIN = ( <W_ZFIT0026>-VLR_JUROS_CALC - <W_ZFIT0026>-VLR_JUROS_RBDO - <W_ZFIT0026>-VLR_DESC_JROS )
                     + ( <W_ZFIT0026>-VLR_MULTA_CALC - <W_ZFIT0026>-VLR_MULTA_RBDO - <W_ZFIT0026>-VLR_DESC_MULT ).


        E_VLR_TOTAL = ( E_VLR_TOTAL + VLR_SALD_FIN ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
