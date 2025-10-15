**&---------------------------------------------------------------------*
**&  Include           ZXWOCU07
**&---------------------------------------------------------------------*
*-----------------------------------------------------------------------*
*                      Controle de Alterações                           *
*-----------------------------------------------------------------------*
* Data      |Request   |Autor       |Alteração                          *
*-----------------------------------------------------------------------*
* 04/08/2025|DEVK9A2ANS|NSEGATIN    |Implem.. Exit Ord. Manut. APP Fiori*
*                                   |Chamado: 186781.                   *
*-----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&  TABELAS WORK-ÁREAS
**&---------------------------------------------------------------------*
*DATA: WA_PLKO  TYPE PLKO,
*      WA_EQUZ  TYPE EQUZ.
***&---------------------------------------------------------------------*
***&  VARIÁVEIS
***&---------------------------------------------------------------------*
*DATA: TEXT000(45), TEXT001 TYPE CHAR45,
*      TEXT002(45), TEXT003 TYPE CHAR45,
*      TEXT004(45), TEXT005 TYPE CHAR45,
*      TEXT006(45), TEXT007 TYPE CHAR45,
*      TEXT008(45), TEXT009 TYPE CHAR45,
*      TEXT010(45), TEXT011 TYPE CHAR45,
*      TEXT012(45), TEXT013 TYPE CHAR45,
*      TEXT014(45).
*
**&---------------------------------------------------------------------*
**&  FIELD-SYMBOLS
**&---------------------------------------------------------------------*
*FIELD-SYMBOLS:  <iloa_imp>    TYPE iloa.   "<<<------"186781 - NMS ------->>>
*               <AFVGD_IMP>   TYPE AFVGD,
*               <RMIPM_IMP>   TYPE RMIPM,
*               <COBRB_IMP>   TYPE COBRB.
*
*ASSIGN:         ('(SAPLCOIH)ILOA')  TO <iloa_imp>.   "<<<------"186781 - NMS ------->>>
*               ('(SAPLCOIH)AFVGD') TO <AFVGD_IMP>,
*               ('(SAPLIWP3)RMIPM') TO <RMIPM_IMP>,
*               ('(SAPLKOBS)COBRB') TO <COBRB_IMP>.
*
**&---------------------------------------------------------------------*
**&  MENSAGENS
**&---------------------------------------------------------------------*
*TEXT000 = 'Centro de trabalho responsável diferente'.
*TEXT001 = 'do centro do grupo planejamento.'.
*TEXT002 = 'Centro de planejamento da operação diferente'.
*TEXT003 = 'do centro do grupo de planejamento.'.
*TEXT004 = 'Centro de localização diferente'.
*TEXT005 = 'do centro do grupo de planejamento.'.
*TEXT006 = 'Divisão não pertence a empresa indicada.'.
*TEXT007 = 'Centro de custo não pertence a divisão indicada.'.
*TEXT008 = 'Centro de planejamento do plano diferente do'.
*TEXT009 = 'centro do equipamento.'.
*TEXT010 = 'Nr Agrupador da lista de tarefas diferente'.
*TEXT011 = 'do centro de planejamento do plano.'.
*TEXT012 = 'Norma de apropriação diferente'.
*TEXT013 = 'do centro de custo.'.
*TEXT014 = 'local de instalação.'.
*
*CASE SY-TCODE.
*  WHEN 'IW31' OR
*       'IW32' OR
*       'IW33' OR
*       'IW34' OR
*       'IW36' OR
*       'IW21'.
*
**   Se ordem estiver liberada e com custo 0
*    IF ( CAUFVD_IMP-STTXT(3) EQ 'LIB' AND CAUFVD_IMP-USER4 NE '0' ).
*      IF CAUFVD_IMP-VAWRK NE CAUFVD_IMP-IWERK.
*        MESSAGE W836(SD) WITH TEXT000 TEXT001.
*      ELSEIF <AFVGD_IMP>-WERKS NE CAUFVD_IMP-IWERK.
**     -
*        IF <AFVGD_IMP>-WERKS IS NOT INITIAL.
*          MESSAGE W836(SD) WITH TEXT002 TEXT003.
*        ELSE.
*          IF CAUFVD_IMP-WERKS NE CAUFVD_IMP-IWERK.
*            MESSAGE W836(SD) WITH TEXT002 TEXT003.
*          ENDIF.
*        ENDIF.
**     -
*      ELSEIF <ILOA_IMP>-SWERK NE CAUFVD_IMP-IWERK.
*        MESSAGE W836(SD) WITH TEXT004 TEXT005.
*      ELSEIF CAUFVD_IMP-GSBER NE CAUFVD_IMP-IWERK.
*        MESSAGE W836(SD) WITH TEXT006.
*      ELSEIF <ILOA_IMP>-GSBER NE CAUFVD_IMP-IWERK.
*        MESSAGE W836(SD) WITH TEXT006.
*      ELSEIF <ILOA_IMP>-KOSTL+4(2) NE <ILOA_IMP>-GSBER+2(2).
*        MESSAGE W836(SD) WITH TEXT007.
*      ELSEIF <COBRB_IMP> IS ASSIGNED
*         AND <COBRB_IMP> IS NOT INITIAL.
*        IF <COBRB_IMP>-KOSTL NE <ILOA_IMP>-KOSTL.
*          MESSAGE W836(SD) WITH TEXT012 TEXT013.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      "Centro de trabalho resp.
*      IF CAUFVD_IMP-VAWRK NE CAUFVD_IMP-IWERK.
*        MESSAGE E836(SD) WITH TEXT000 TEXT001.
*        "Centro de planejamento da operação
*      ELSEIF <AFVGD_IMP>-WERKS NE CAUFVD_IMP-IWERK.
**     -
*        IF <AFVGD_IMP>-WERKS IS NOT INITIAL.
*          MESSAGE E836(SD) WITH TEXT002 TEXT003.
*        ELSE.
*          IF CAUFVD_IMP-WERKS NE CAUFVD_IMP-IWERK.
*            MESSAGE E836(SD) WITH TEXT002 TEXT003.
*          ENDIF.
*        ENDIF.
**     -
*        "Centro de localização
*      ELSEIF <ILOA_IMP>-SWERK NE CAUFVD_IMP-IWERK.
*        MESSAGE E836(SD) WITH TEXT004 TEXT005.
*        "Divisão dos dados adicionais
*      ELSEIF CAUFVD_IMP-GSBER NE CAUFVD_IMP-IWERK.
*        MESSAGE E836(SD) WITH TEXT006.
*        "Divisão da localização
*      ELSEIF <ILOA_IMP>-GSBER NE CAUFVD_IMP-IWERK.
*        MESSAGE E836(SD) WITH TEXT006.
*        "Centro de custo
*      ELSEIF <ILOA_IMP>-KOSTL+4(2) NE <ILOA_IMP>-GSBER+2(2).
*        MESSAGE E836(SD) WITH TEXT007.
*        "Norma de apropriação
*      ELSEIF <COBRB_IMP> IS ASSIGNED
*         AND <COBRB_IMP> IS NOT INITIAL.
*        IF <COBRB_IMP>-KOSTL NE <ILOA_IMP>-KOSTL.
*          MESSAGE E836(SD) WITH TEXT012 TEXT013.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  WHEN 'IP10'.
*
** Seleciona os dados das listas de tarefas.
*    SELECT SINGLE *
*      FROM PLKO
*      INTO WA_PLKO
*     WHERE PLNNR EQ <RMIPM_IMP>-PLNNR
*       AND PLNAL EQ <RMIPM_IMP>-PLNAL.
*
** Seleciona os dados do equipamento
*    SELECT SINGLE *
*      FROM EQUZ
*      INTO WA_EQUZ
*     WHERE EQUNR EQ CAUFVD_IMP-EQUNR
*       AND DATBI EQ '99991231'.
*
*    IF WA_EQUZ IS NOT INITIAL.
*      IF WA_EQUZ-IWERK NE CAUFVD_IMP-WERKS.
*        MESSAGE E836(SD) WITH TEXT008 TEXT009.
*      ENDIF.
*    ELSE.
*      IF <ILOA_IMP>-SWERK NE CAUFVD_IMP-WERKS.
*        MESSAGE E836(SD) WITH TEXT008 TEXT014.
*      ENDIF.
*    ENDIF.
*
*    IF CAUFVD_IMP-WERKS NE CAUFVD_IMP-VAWRK.
*      MESSAGE E836(SD) WITH TEXT000 TEXT001.
*    ELSEIF WA_PLKO-IWERK NE CAUFVD_IMP-WERKS.
*      MESSAGE E836(SD) WITH TEXT010 TEXT011.
*    ELSEIF <RMIPM_IMP>-GSBER NE <RMIPM_IMP>-IWERK.
*      MESSAGE E836(SD) WITH TEXT006.
*    ENDIF.
*  WHEN OTHERS.
*ENDCASE.

********************************************************************
* CONSISTÊNCIA NAS TRANSAÇÕES IW31 E IW32 PARA ORDENS TIPO 30 (PM) *
* INÍCIO                                                           *
********************************************************************
*.COBRB-Puffer mit Änderungsflag
types: begin of ty_cobrb_buf.
         include structure cobrb.
types:   uflag like dkobr-upd_flag,
       end of ty_cobrb_buf.

types: begin of ty_afvgd.
         include structure afvgb.
types: end of ty_afvgd.

types: begin of ty_resb.
         include structure resbb.
types: end of ty_resb.

types: begin of ty_ix_esll.
         include structure msupdap.
types:   selkz like rm11p-selkz.
types: end of ty_ix_esll.

*** US #170302 - MMSILVA - 24.06.2025 - Ini ***
types: begin of ty_aufk.
         include structure aufk.
types: end of ty_aufk.
*** US #170302 - MMSILVA - 24.06.2025 - Fim ***

data: it_afvgd       type standard table of ty_afvgd,
      wa_afvgd       type ty_afvgd,
      it_resb        type standard table of ty_resb,
      wa_resb        type ty_resb,
      it_resb_aux    type standard table of ty_resb,
      wa_resb_aux    type ty_resb,
      it_cobrb       type table of ty_cobrb_buf,
      wa_cobrb       type ty_cobrb_buf,
      it_resb_atend  type standard table of ty_resb,
      wa_resb_atend  type ty_resb,
      it_afvv        type standard table of afvv,
      wa_afvv        type afvv,
      it_ix_esll     type standard table of ty_ix_esll,
      it_ix_esll_mm  type standard table of ty_ix_esll,
      wa_ix_esll     type ty_ix_esll,
      wa_ix_esll2    type ty_ix_esll,
      wa_ix_esll_aux type ty_ix_esll,
      vg_atwrt       type ausp-atwrt,
      vg_matnr       type eban-matnr,
      ck_odata       type char1,
      msg01          type string,
      msg02          type string,
      msg03          type string,
      msg04          type string,
      msg05          type string.

data: t_afvgd type standard table of ty_afvgd,
      msg_    type string.

field-symbols: <f_prctr>  type any,
               <ft_caufv> type table,
               <prctr>    type prctr,
               <kostl>    type kostl,
               <aufnr>    type aufnr,
               <fw_caufv> type dfps_afvg_bt.

field-symbols: <afvgd>   type any,
               <cobrb>   type table,
               <resb>    type any,
               <ix_esll> type any.

data: i_aufnr type aufk-aufnr.
data: w_ihgns type ihgns.        "<<<------"186781 - NMS ------->>>

*** US #170302 - MMSILVA - 24.06.2025 - Ini ***
data: it_aufk   type standard table of ty_aufk,
      ls_aufk   type ty_aufk,
      var_check type char01.
*** US #170302 - MMSILVA - 24.06.2025 - Fim ***

clear: var_check.

*-US 158036-26-11-2024-#158036-RJF-Início
**<<<------"186781 - NMS - INI------>>>
*if sy-tcode eq 'IW31' or sy-tcode eq 'IW32' or sy-tcode eq 'IW34'.
if sy-tcode eq 'IW31' or sy-tcode eq 'IW32' or sy-tcode eq 'IW34' or
  ( sy-tcode is initial    and
    sy-cprog eq 'SAPMHTTP' and
  ( trtyp    eq 'H'        or   "Criar
    trtyp    eq 'V' ) ).        "Modificar
**<<<------"186781 - NMS - FIM------>>>
  data: ln_kostln(4)  type n,
        ln_kostlm(10) type n.


  assign ('(SAPLCOIH)ILOA-KOSTL') to <kostl>.
  if <kostl> is assigned.
    ln_kostlm = <kostl>.
  endif.


  "bug impeditivo - 186327 - RGA
  data: esheader  type bapi_alm_order_header_e,
        it_return type table of bapiret2,
        it_olist  type table of bapi_alm_order_objectlist,
        isheader  type table of bapi_alm_order_headers_i.


  if caufvd_imp-equnr is not initial and
     caufvd_imp-auart <> 'SI01'. "FF #190516

    select equnr, eqtyp, eqart
      up to 1 rows
      from itob " *ITOB-EQTYP - Objetos técnicos PM (EQUI, local de instalação)
      into @data(wa_itob)
      where equnr eq @caufvd_imp-equnr.
    endselect.
    if sy-subrc is initial.

      if  wa_itob-eqtyp eq '1'
       or wa_itob-eqtyp eq '2'
       or wa_itob-eqtyp eq '3'
       or wa_itob-eqtyp eq '4'.
*       or wa_itob-eqtyp eq 'A'. "FF #190614

        select * from zpmt0001
        into table @data(it_ZPMT0001)
        where eqtyp  eq @wa_itob-eqtyp
          and eqart  eq @wa_itob-eqart.
*          AND kostlg EQ @ln_kostlm+6(4).

        if sy-subrc is initial.
          sort it_ZPMT0001 by eqtyp eqart kostlg.
          loop at it_ZPMT0001 into data(wa_PMT0001) where eqtyp = wa_itob-eqtyp
                                                      and eqart = wa_itob-eqart.
            ln_kostln  = wa_PMT0001-kostlg.
            if sy-subrc is not initial or ln_kostln ne ln_kostlm+6(4).
              msg01 = 'Verificar categoria informada x centro de custos tabela ZPM0010.'.
              message  msg01 type 'E'.
            endif.
          endloop.

        else.
          msg01 = 'Verificar categoria informada x centro de custos tabela ZPM0010.'.
          message  msg01 type 'E'.
        endif.
      endif.
    endif.
  endif.
endif.
*-US 158036-26-11-2024-#158036-RJF-Fim

if caufvd_imp-autyp eq '30'.
*-US 138138-23-07-2024-#138138-RJF-inicio
**<<<------"186781 - NMS - INI------>>>
*  IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34'.
  if sy-tcode eq 'IW31' or sy-tcode eq 'IW32' or sy-tcode eq 'IW34' or
    ( sy-tcode is initial    and
      sy-cprog eq 'SAPMHTTP' and
    ( trtyp    eq 'H'        or   "Criar
      trtyp    eq 'V' ) ).        "Modificar
**<<<------"186781 - NMS - FIM------>>>
    data: ln_kostl(10) type n,
          lv_aufnr     type aufnr.

    assign ('(SAPLCOIH)ILOA-KOSTL') to <kostl>.
    if <kostl> is assigned.
      ln_kostl = <kostl>.
    endif.
    assign ('(SAPLCOIH)ILOA-AUFNR') to <aufnr>.
    if <aufnr> is assigned.
      lv_aufnr = <aufnr>.
    endif.

    if ln_kostl+7(3) = '196'.
      select *
        up to 1 rows
        into @data(wa_aufk)
        from coas
        where autyp eq '01'
          and ( auart eq 'ZSTA' or auart eq 'ZOAN' )
          and phas1 eq @abap_true
          and werks eq @caufvd_imp-werks.
      endselect.
      if sy-subrc is initial.
        if lv_aufnr is initial.
          msg01 = 'Campo Ord. apropriação é obrigatório!'.
          message  msg01 type 'E'.
        endif.
      endif.

      if lv_aufnr is not initial.
        select *
          up to 1 rows
          into @wa_aufk
          from coas
          where aufnr eq @lv_aufnr
            and autyp eq '01'
            and ( auart eq 'ZSTA' or auart eq 'ZOAN' )
            and phas1 eq @abap_true
            and werks eq @caufvd_imp-werks.
        endselect.
        if sy-subrc ne 0.
          msg01 = 'Verificar ordem informada, dados incorretos!'.
          message  msg01 type 'E'.
        endif.
      endif.
    endif.
  endif.
*-US 138138-23-07-2024-#138138-RJF-fim

  case sy-tcode.
**<<<------"186781 - NMS - INI------>>>
*    WHEN 'IW31' OR 'IW32' OR 'IW34' OR 'IW21' OR 'IW22'. "FF - 22.12.2023 - BUG SOLTO 129760
    when 'IW31' or 'IW32' or 'IW34' or 'IW21' or 'IW22' or space.
      if   sy-tcode ne space      or
         ( sy-tcode eq space      and
           sy-cprog eq 'SAPMHTTP' and
         ( trtyp    eq 'H'        or   "Criar
           trtyp    eq 'V' ) ).        "Modificar
**<<<------"186781 - NMS - FIM------>>>
*    WHEN 'IW31' OR 'IW32' OR 'IW34'.

        if caufvd_imp-sttxt(3) eq 'LIB'.
          move abap_true to ck_odata.
        endif.
**<<<------"186781 - NMS - INI------>>>
      endif.
**<<<------"186781 - NMS - FIM------>>>

*      SELECT SINGLE C~PRCTR
*      FROM AFIH AS A
*           INNER JOIN ILOA AS B ON A~ILOAN EQ B~ILOAN
*           INNER JOIN CSKS AS C ON B~KOSTL EQ C~KOSTL
*        INTO @DATA(V_LRCTR)
*      WHERE A~AUFNR EQ @CAUFVD_IMP-AUFNR
*        AND A~ILOAN EQ B~ILOAN
*        AND B~KOSTL EQ C~KOSTL.
*
*      IF SY-SUBRC IS NOT INITIAL.
*
*        ASSIGN ('(SAPLCOIH)ILOA-KOSTL') TO <KOSTL>.
*
*        IF <KOSTL> IS ASSIGNED.
*
*          SELECT SINGLE PRCTR
*           FROM CSKS
*           INTO V_LRCTR
*           WHERE KOSTL EQ <KOSTL>.
*
*        ENDIF.
*      ENDIF.
*
*      ASSIGN ('(SAPLCOBP)AFVG_BT[]') TO <F_PRCTR>.
*
*      IF <F_PRCTR> IS ASSIGNED.
*        T_AFVGD = <F_PRCTR>.
*        LOOP AT T_AFVGD ASSIGNING FIELD-SYMBOL(<F_AFVGD>).
*          MSG_ = COND #( WHEN <F_AFVGD>-PRCTR NE V_LRCTR
*                               THEN COND #( WHEN MSG_ IS INITIAL
*                                            THEN <F_AFVGD>-VORNR
*                                            ELSE |{ MSG_ }, { <F_AFVGD>-VORNR }|
*                                         )
*                       ELSE |{ MSG_ }| ).
*        ENDLOOP.
*
*        IF MSG_ IS NOT INITIAL.
*
*          DATA(V_LEN) = STRLEN( MSG_ ).
*
*          IF V_LEN > 23.
*            MSG_ = MSG_(23).
*            MSG_ = |{ MSG_ }...|.
*          ENDIF.
*
*          IF LINES( T_AFVGD[] ) EQ 1.
*            MSG02 = | O Centro de Lucro da Operação: { MSG_ } é diferente do Centro de Lucro { V_LRCTR } do Equipamento !|.
*          ELSE.
*            MSG02 = | O Centro de Lucro das Operações: { MSG_ } são diferentes do Centro de Lucro { V_LRCTR } do Equipamento!|.
*          ENDIF.
*          MESSAGE MSG02 TYPE 'E'.
*        ENDIF.
*
*      ENDIF.


  endcase.

endif.

assign ('(SAPLMLSP)IX_ESLL[]') to <ix_esll>.
if <ix_esll> is assigned.
  it_ix_esll    = <ix_esll>.
  it_ix_esll_mm = <ix_esll>.
endif.

assign ('(SAPLCOBP)AFVG_BT[]') to <afvgd>.
check <afvgd> is assigned.
it_afvgd = <afvgd>.


*=Comentado por AOENNING 12/11/2018 - Para as ordens que estão com WERKS <> GSBER diferentes possam ser encerradas =====.
*-----------------------------------------------------------------------------------------------------------------------

*Loop nas operações não eliminadas*

*--> CS1064258 / IR126350 -. Não validar se o usuário estiver no SET
*--> Retirar usuário do set após encerramento da ordem.

data: v_check(1) type c value 1,
      v_data     type sy-datum.

select single *
    from setleaf into @data(v_ernam)
     where setclass eq '0000'
       and subclass eq ''
       and setname eq 'PM_IWXX'.
if sy-subrc eq 0.

  select single * from setlinet into @data(linet)
    where setclass eq '0000'
         and subclass eq ''
         and setname eq 'PM_IWXX'.

  if sy-subrc eq 0.
    v_data = linet-descript(8).
    if v_ernam-valfrom eq sy-uname and sy-datum le v_data..
      v_check = 0.
    endif.
  endif.
endif.

*<-- CS1064258 / IR126350

loop at it_afvgd into wa_afvgd where phflg ne abap_true.
  "Checa se divisão esta OK
  if wa_afvgd-gsber is not initial. "FF - 19/04/24 - #131818
    if wa_afvgd-gsber ne caufvd_imp-iwerk and v_check eq 1.
      concatenate 'Não é possível salvar. Divisão' wa_afvgd-gsber 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
      message  msg01 type 'E'.
    endif.
  endif.

  if caufvd_imp-vawrk ne caufvd_imp-iwerk and v_check eq 1.
    concatenate 'Não é possível salvar. Centro Resp.' caufvd_imp-vawrk 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
    message  msg01 type 'E'.
  endif.
  "
  if wa_afvgd-werks ne caufvd_imp-iwerk and v_check eq 1.
    concatenate 'Não é possível salvar. Centro Operação.' wa_afvgd-werks 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
    message  msg01 type 'E'.
  endif.
**<<<------"186781 - NMS - INI------>>>
  "Verifica se orçamento inicial esta sendo alterado e se existe orçamento ja aprovado.
  "Localizar ordem pela operação.
  clear: i_aufnr, w_ihgns.
  select single *
  from ihgns
    into w_ihgns
  where objnr eq caufvd_imp-objnr and geniakt ne abap_true.

  "Se w_ihgns estiver preenchida é porque ja existe orçamento inicial aprovado para a ordem.
  if w_ihgns is not initial.
    "Verifica o valor inicial aprovado AUFK.
    select single * from aufk into @data(ws_aufk) where objnr eq @caufvd_imp-objnr.
    if ws_aufk is not initial and v_check eq 1.
      if ws_aufk-user4 ne caufvd_imp-user4.
        msg01 = 'Não é possível salvar. A ordem ->' && ws_aufk-aufnr && ' Ja possuem um nivél aprovado para orçamento informado.'.
        message  msg01 type 'E'.
      endif.
    endif.
  endif.
  clear: ws_aufk.

**//====================Comentado 19-08-2025 para uma nova analise / AOENNING
* Verifica se orçamento inicial esta sendo alterado e se existe orçamento ja aprovado.
* Localizar ordem pela operação.
*  SELECT a~objnr, a~counter, pmsog, genvname, gendatum, gentime
*    FROM ihsg AS a
*    INNER JOIN ihgns AS b
*     ON a~objnr   EQ b~objnr   AND
*        a~counter EQ b~counter
*    INTO TABLE @DATA(tl_adm_licenca)
*  WHERE a~objnr EQ @caufvd_imp-objnr.
** Se TL_ADM_LICENCA estiver preenchida é porque já existe orçamento inicial aprovado para a ordem.
*  IF sy-subrc IS INITIAL.
*    msg01 = 'Não é possível salvar. A ordem ->' && caufvd_imp-aufnr && ' Ja possuem um nivél aprovado para orçamento informado.'.
*    MESSAGE msg01 TYPE 'E'.
*
*  ENDIF.
**<<<------"186781 - NMS - FIM------>>>
*//===========================//====================================================
endloop.

"Apos analise da area, suspendeu esse bloqueio devido influenciar no processo.
*"Verifica permissão acesso.
*  "USER STORY 96114 / Anderson Oenning
*  "Verificar empresa.
*  SELECT SINGLE * FROM j_1bbranch INTO @DATA(ws_1bbranch) WHERE branch EQ @caufvd_imp-werks.
*  IF ws_1bbranch-bukrs IS NOT INITIAL.
*    "CHECK SET.
*    SELECT SINGLE *
*      FROM setleaf
*      INTO @DATA(i_data)
*        WHERE setname EQ 'MAGI_PM_PERM_IW31'
*          AND valfrom EQ @ws_1bbranch-bukrs.
*
*    IF sy-subrc EQ 0.
*      IF sy-tcode EQ  'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34'.
*        SELECT SINGLE * FROM zpmt0068 INTO @DATA(it_zpmt0068) WHERE auart EQ @caufvd_imp-auart AND usuario EQ @SY-uname.
*        IF sy-subrc NE 0.
*          MESSAGE e461(iw) WITH 'Usuario sem permissão para esse tipo de ordem, '
*                                'realizar o cadastro na transação ZPM0088'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.


"
delete it_ix_esll_mm where srvpos is initial.
if it_ix_esll_mm is not initial.
  loop at it_afvgd into wa_afvgd where phflg ne abap_true.
    if wa_afvgd-steus ne 'PM03' and wa_afvgd-steus ne 'PM05'.
      continue.
    endif.
    if wa_afvgd-banfn is not initial.
      select single *
        from eban
        into @data(_eban)
        where banfn = @wa_afvgd-banfn
        and   bnfpo = @wa_afvgd-bnfpo.

      if sy-subrc eq 0.
        continue.
      endif.
    endif.

    read table it_ix_esll into wa_ix_esll2 with key packno = wa_afvgd-packno.
    if sy-subrc ne 0.
      continue.
    endif.
    loop  at it_ix_esll_mm into wa_ix_esll where packno = wa_ix_esll2-sub_packno.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_ix_esll-srvpos
        importing
          output = vg_atwrt.
      select single *
        from ausp
        into @data(_ausp)
        where klart = '200'
        and   atwrt = @vg_atwrt.
      if sy-subrc ne 0.
        concatenate 'Servico ' vg_atwrt 'não tem material correspondente, contacte o SUPRIMENTOS' into msg01 separated by space.
        message  msg01 type 'E'.
      else.
        vg_matnr = _ausp-objek+0(18).
        select single *
          from makt
          into @data(_makt)
          where matnr = @vg_matnr
          and   spras = @sy-langu.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = vg_matnr
          importing
            output = vg_matnr.
        concatenate vg_atwrt '->'vg_matnr '-' _makt-maktx  into msg01 separated by space.
        message  msg01 type 'W'.
      endif.
    endloop.
  endloop.
endif.

assign ('(SAPLKOBS)GT_COBRB_BUF[]') to <cobrb>.

if <cobrb> is assigned.
  it_cobrb = <cobrb>.
endif.

data vcont  type i value is initial.
data vperbz type cobrb-perbz.

loop at it_cobrb  into wa_cobrb
*---> CS1115375 / IR143642 --->
    where bukrs eq wa_afvgd-bukrs.
*<--- CS1115375 / IR143642 <---
  add 1 to vcont.
  select single *
    from csks
    into @data(_csks)
    where kokrs = @wa_cobrb-kokrs
    and   kostl = @wa_cobrb-kostl.
  if wa_cobrb-kostl is not initial.
    if _csks-gsber ne caufvd_imp-iwerk and v_check eq 1.
      concatenate 'Não é possível salvar. Divisão' _csks-gsber 'não pertence ao centro de Plan.' caufvd_imp-iwerk into msg01 separated by space.
      message  msg01 type 'E'.
    endif.
  elseif wa_cobrb-objnr is not initial.
    select single a~werks
    from afvc as a
      into @data(vg_centro)
      where objnr eq @wa_cobrb-objnr.
    if sy-subrc ne 0.
      "Verifica na tabela coas.
      select single gsber from coas into vg_centro
        where ( objnr eq  wa_cobrb-objnr ) or ( objnr = wa_cobrb-rec_objnr1 ).
    endif.

    if vg_centro ne caufvd_imp-iwerk.
      concatenate 'Não é possível salvar. Divisão' vg_centro 'não pertence ao centro de Plan.' caufvd_imp-iwerk into msg01 separated by space.
      message  msg01 type 'E'.
    endif.
  endif.

  if vcont = 2 and vperbz = wa_cobrb-perbz and v_check eq 1 and caufvd_imp-auart ne 'PJ01'.
    concatenate 'Não é possível salvar. Distribuição da Norma de apropriação incorreta para Operação' wa_cobrb-objnr+18(3) into msg01 separated by space.
    message  msg01 type 'E'.
  elseif vcont = 2.
    vcont = 0.
  endif.

  vperbz = wa_cobrb-perbz.

  if wa_cobrb-lfdnr gt 2 and v_check eq 1.
    concatenate 'Norma de apropriação incorreta para operação ' wa_cobrb-objnr+18(3) 'núm.máximo permitido é de 002 regras ' into msg01 separated by space.
    message  msg01 type 'E'.
  endif.


endloop.

"FF - 20.12.2023 - inicio - BUG SOLTO 129760
if v_check eq 1.

  if sy-tcode = 'IW31' or
     sy-tcode = 'IW32' or
     sy-tcode = 'IW34' or
     sy-tcode = 'IW21' or
**<<<------"186781 - NMS - INI------>>>
*     sy-tcode = 'IW22'.
     sy-tcode = 'IW22' or
     ( sy-tcode eq space      and
       sy-cprog eq 'SAPMHTTP' and
     ( trtyp    eq 'H'        or   "Criar
       trtyp    eq 'V' ) ).        "Modificar
**<<<------"186781 - NMS - FIM------>>>
    if caufvd_imp-aufnr is not initial.

      select single aufpl
      into @data(lv_aufpl)
      from afko
      where aufnr = @caufvd_imp-aufnr.

      if sy-subrc = 0.

        select objnr
        from afvc
        into table @data(lt_afvc)
        where aufpl = @lv_aufpl.

        if sy-subrc = 0 and lt_afvc is not initial.

          select *
          from cobrb
          into table @data(it_cobrb_aux)
          for all entries in @lt_afvc
          where objnr = @lt_afvc-objnr.

          if sy-subrc = 0.
            data(lv_tam_buff) = lines( it_cobrb_aux ).
            data(lv_tam) = lines( it_cobrb ).

            if lv_tam_buff = lv_tam.

            else.

              loop at it_cobrb_aux assigning field-symbol(<fs_aux>).
                read table it_cobrb with key objnr = <fs_aux>-objnr
                                             lfdnr = <fs_aux>-lfdnr transporting no fields.
                if sy-subrc <> 0.
                  append initial line to it_cobrb assigning field-symbol(<fs>).
                  <fs> = <fs_aux>.
                endif.
              endloop.

            endif.

          endif.

        endif.
      endif.
    endif.


    loop at it_cobrb assigning field-symbol(<ws_cobrb>).
      if <ws_cobrb>-kostl is not initial.
        if <ws_cobrb>-kostl <> caufvd_imp-kostl. "Não é permitido centro de custos diferentes.
          msg01 = 'Não é possível salvar. Norma de apropriação divergentes.'.
          message  msg01 type 'E'.
        endif.
      endif.
    endloop.
**<<<------"186781 - NMS - INI------>>>
*    IF caufvd_imp-iwerk <> <iloa_imp>-swerk.
*      CONCATENATE 'Centro do grupo planejamento' caufvd_imp-iwerk 'diferente do Centro de manutenção' <iloa_imp>-swerk INTO msg01 SEPARATED BY space.
    if caufvd_imp-iwerk <> caufvd_imp-swerk.
      concatenate 'Centro do grupo planejamento' caufvd_imp-iwerk 'diferente do Centro de manutenção' caufvd_imp-swerk into msg01 separated by space.
**<<<------"186781 - NMS - FIM------>>>
      message  msg01 type 'E'.
    endif.
**<<<------"186781 - NMS - INI------>>>
*    IF caufvd_imp-iwerk <> <iloa_imp>-gsber.
*      CONCATENATE 'Centro do grupo planejamento' caufvd_imp-iwerk 'diferente da divisão' <iloa_imp>-gsber INTO msg01 SEPARATED BY space.
    if caufvd_imp-iwerk <> caufvd_imp-igsber.
      concatenate 'Centro do grupo planejamento' caufvd_imp-iwerk 'diferente da divisão' caufvd_imp-igsber into msg01 separated by space.
**<<<------"186781 - NMS - FIM------>>>
      message  msg01 type 'E'.
    endif.

    if caufvd_imp-iwerk <> caufvd_imp-gsber.
      concatenate 'Centro do grupo planejamento' caufvd_imp-iwerk 'diferente da divisão' caufvd_imp-gsber into msg01 separated by space.
      message  msg01 type 'E'.
    endif.

  endif.
endif.
"FF - 20.12.2023 - fim


assign ('(SAPLCOBC)RESB_BT[]') to <resb>.
if <resb> is assigned.
  it_resb = <resb>.
endif.
loop at it_resb into wa_resb_aux where vbkz eq 'I'.
  if wa_resb_aux-werks  ne caufvd_imp-iwerk and v_check eq 1.
    concatenate 'Não é possível salvar. Centro Componente.' wa_resb_aux-werks 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
    message  msg01 type 'E'.
  endif.
endloop.

*=Comentado por AOENNING 12/11/2018 - Para as ordens que estão com WERKS <> GSBER diferentes possam ser encerradas =====.
*-----------------------------------------------------------------------------------------------------------------------
*
*ASSIGN ('(SAPLCOIH)ILOA')  TO <ILOA_IMP>.
*IF <ILOA_IMP> IS ASSIGNED.
*
*ENDIF.

if ck_odata eq abap_true.

  assign ('(SAPLCOBC)RESB_BT[]') to <resb>.
  if <resb> is assigned.
    it_resb = <resb>.
  endif.
  "


*=Comentado por AOENNING 12/11/2018 - Para as ordens que estão com WERKS <> GSBER diferentes possam ser encerradas =====.
*-----------------------------------------------------------------------------------------------------------------------
  "Loop nas operações não eliminadas
  if sy-uname ne 'WSSE_SE'.
    loop at it_afvgd into wa_afvgd where phflg ne abap_true.
      if wa_afvgd-vbkz eq 'I'
         and ( wa_afvgd-ntanf lt sy-datum or wa_afvgd-einsa ne '1' ).
        "Consistência nas op. novas para garantir preenchimento de data de restrição
        concatenate 'Não é possível salvar. Operação' wa_afvgd-vornr 'com data inválida.' into msg01 separated by space.
        message  msg01 type 'E'.
      else.

**&----------------Inicio / Bug Impeditivo 150818 / AOENNING&*
*      if wa_afvgd-steus eq 'PM04'.
*
*        read table it_resb into data(ws_resb) with key vornr = wa_afvgd-vornr.
*
*        if sy-subrc eq 0.
*          if ws_resb-lifnr is initial.
*            msg01 = 'Obrigatório fornecedor para operações com chave PM04.'.
*            message  msg01 type 'E'.
*          endif.
*        endif.
*      endif.
**&----------------Fim / Bug Impeditivo 150818 / AOENNING&*

        "Checa se divisão esta OK
        if wa_afvgd-gsber ne caufvd_imp-iwerk and v_check eq 1.
          concatenate 'Não é possível salvar. Divisão' wa_afvgd-gsber 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
          message  msg01 type 'E'.
        endif.
        if caufvd_imp-vawrk ne caufvd_imp-iwerk.
          concatenate 'Não é possível salvar. Centro Resp.' caufvd_imp-vawrk 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
          message  msg01 type 'E'.
        endif.
        "
        if wa_afvgd-werks ne caufvd_imp-iwerk and v_check eq 1.
          concatenate 'Não é possível salvar. Centro Operação.' wa_afvgd-werks 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
          message  msg01 type 'E'.
        endif.

        select *
          from afvv
          into table it_afvv
          where aplzl = wa_afvgd-aplzl
          and aufpl = wa_afvgd-aufpl.

        read table it_afvv into wa_afvv with key aplzl = wa_afvgd-aplzl
                                                 aufpl = wa_afvgd-aufpl.

        it_resb_aux = it_resb.

        if it_resb_aux is not initial.
          delete it_resb_aux where vornr ne wa_afvgd-vornr.
          "AND ( XLOEK EQ ABAP_TRUE OR VBKZ EQ 'X' OR VBKZ EQ 'D' ).
          it_resb_atend = it_resb.
          delete it_resb_atend
            where vornr ne wa_afvgd-vornr
            or vbkz eq 'X'
            or vbkz eq 'D'
            or xloek eq abap_true.
          delete it_resb_atend
            where kzear ne abap_true
            and enmng eq 0.
        endif.
        "Consistências para troca de data nas operações. Só entra nas consistências se a troca involver o passado.
        if ( wa_afvv-ntanf ne wa_afvgd-ntanf or wa_afvv-einsa ne wa_afvgd-einsa ) and not ( wa_afvv-ntanf ge sy-datum and wa_afvgd-ntanf ge sy-datum ).
          "Consistência para saber se houve alteração de data de operação com material atendido.
          if it_resb_atend is not initial .
            concatenate 'Não é possível salvar. Há componente já atendido na Operação' wa_afvgd-vornr into msg02 separated by space.
            message msg02 type 'E'.
          endif.
          "Consistência para saber se houve alteração de data de operação com serviço já associado anteriormente.
          if it_ix_esll is not initial.
            read table it_ix_esll into wa_ix_esll_aux with key packno = wa_afvgd-packno.
            if sy-subrc eq 0.
              loop at it_ix_esll into wa_ix_esll where packno eq wa_ix_esll_aux-sub_packno.
                if wa_ix_esll-srvpos is not initial and wa_ix_esll-kz is initial.
                  concatenate 'Não é possível salvar. Operação' wa_afvgd-vornr 'teve serviço já vínculado' into msg03 separated by space.
                  message msg03 type 'E'.
                endif.
              endloop.
            endif.
          endif.
          "Consistência para saber se houve inclusão de componente em operação com data no passada.
          loop at it_resb_aux into wa_resb_aux where vbkz eq 'I'.
            if wa_afvgd-ntanf lt sy-datum .
              concatenate 'Não é possível salvar Componente' wa_resb_aux-posnr 'na Operação' wa_afvgd-vornr into msg04 separated by space.
              message msg04 type 'E'.
            endif.
            if wa_resb_aux-werks  ne caufvd_imp-iwerk and v_check eq 1.
              concatenate 'Não é possível salvar. Centro Componente.' wa_resb_aux-werks 'diferente do Centro de Plan.' caufvd_imp-iwerk  into msg01 separated by space.
              message  msg01 type 'E'.
            endif.
          endloop.
        endif.
        "Consistência para verificar se está incluindo serviço em uma operação do passado.
        if it_ix_esll is not initial.
          read table it_ix_esll into wa_ix_esll_aux with key packno = wa_afvgd-packno.
          if sy-subrc eq 0.
            loop at it_ix_esll into wa_ix_esll where packno eq wa_ix_esll_aux-sub_packno.
              if wa_ix_esll-srvpos is not initial and wa_ix_esll-kz eq 'I'  and ( wa_afvgd-einsa ne '1' or wa_afvgd-ntanf lt sy-datum ).
                concatenate 'Não é possível salvar. Operação' wa_afvgd-vornr 'com data inválida ou não informada' into msg05 separated by space.
                message msg05 type 'E'.
              endif.
            endloop.
          endif.
        endif.

      endif.
    endloop.
  endif.
*

*---> CS10559788 - IR124857 / CS1065760 - IR127024
* --> ROTINA PARA GRAVACAO

  data: v_qtd   like resbb-rspos,
        v_vornr like resbb-vornr,
        v_rsnum like resbb-rsnum,
        v_aufnr like resbb-aufnr,
        v_resb  like resbb,
        v_ebkn  like ebkn,
        v_eban  like eban,
        v_coupa type resbb-rssta,
        w_resb  type resb,
        v_user  type setleaf,
        v_linet type setlinet.

  v_check = 1.
*---> ignora regras coupa JAMEDICI --->

  select single *
      from setleaf into v_user
       where setclass eq '0000'
         and subclass eq ''
         and setname eq 'PM_COUPA_IGNORA'.

  if sy-subrc eq 0.

    select single * from setlinet into v_linet
      where setclass eq '0000'
           and subclass eq ''
           and setname eq 'PM_COUPA_IGNORA'.

    if sy-subrc eq 0.
      v_data = v_linet-descript(8).
      if v_user-valfrom eq sy-uname and sy-datum le v_data.
        v_check = 2.
      endif.
    endif.
  endif.

*<--- ignora regras coupa JAMEDICI <---

  if v_check ne 2.

    select single *
      from setleaf into v_user
       where setclass eq '0000'
         and subclass eq ''
         and setname eq 'PM_COUPA'.

    if sy-subrc eq 0.

      select single * from setlinet into v_linet
        where setclass eq '0000'
             and subclass eq ''
             and setname eq 'PM_COUPA'.

      if sy-subrc eq 0.
        v_data = v_linet-descript(8).
        if v_user-valfrom eq sy-uname and sy-datum le v_data.
          v_check = 0.
        endif.
      endif.
    endif.

    assign ('(SAPLCOBC)RESB_BT[]') to <resb>.

    it_resb = <resb>.

    read table it_resb index 1 into v_resb.
    v_vornr = v_resb-vornr.
    v_rsnum = v_resb-rsnum.


    select single * from  ebkn into v_ebkn where aufnr eq v_resb-aufnr.

    if sy-subrc eq 0.
      select single * from eban into v_eban where banfn eq v_ebkn-banfn.

      if sy-subrc eq 0.
        if v_eban-status_coupa is not initial and v_check eq 1.
          v_coupa  = 'X'.
          if v_check eq 0.
            message 'Requisição de compra já aprovada no COUPA - Alteração não permitida.' type 'S' display like 'E'.
            raise no_changes_allowed.
          endif.
        endif.
      endif.

      if v_check eq 1.
        loop at it_resb into v_resb.

          if v_resb-postp ne 'L' and v_resb-xloek ne 'X'.
            if ( v_aufnr eq v_resb-aufnr ) and ( v_vornr ne v_resb-vornr ).
              v_qtd = 1.
              v_aufnr = v_resb-aufnr. v_vornr = v_resb-vornr.
            else.
              v_qtd = v_qtd + 1.
            endif.
          endif.

          if v_qtd gt 99.
            message 'Utilizar nova operação, limite de itens para a mesma operação é de 100 itens' type 'S' display like 'E'.
            raise no_changes_allowed.
          endif.

          "FF - 01/12/2023 - inicio - IR161576
*          SELECT SINGLE * FROM resb INTO w_resb WHERE rsnum EQ v_resb-rsnum AND vornr EQ v_resb-vornr.
          select single *
          from resb
          into w_resb
          where rsnum = v_resb-rsnum
            and rspos = v_resb-rspos
            and posnr = v_resb-posnr
            and vornr = v_resb-vornr
            and sbter => sy-datum.
          "FF - 01/12/2023 - fim

          if sy-subrc eq 0.
            if w_resb-bdmng ne v_resb-bdmng and v_resb-postp ne 'L' and v_coupa eq 'X'.
              message 'Requisição de compra já aprovada no COUPA - Alteração não permitida.' type 'S' display like 'E'.
              raise no_changes_allowed.
            endif.
          endif.
        endloop.
      endif.
    endif.

  endif.
*<--- CS10559788 - IR124857 / CS1065760 - IR127024

*=Comentado por AOENNING 12/11/2018 - Para as ordens que estão com WERKS <> GSBER diferentes possam ser encerradas =====.
*-----------------------------------------------------------------------------------------------------------------------

endif.

"FF - 11/04/2024 - #136140 - inicio
data lv_tcode type sy-tcode.
import lv_tcode to lv_tcode from memory id 'ZPM0026'. "Export feito no programa ZPMR0016_FORMS
"FF - 11/04/2024 - #136140 - fim

if ( sy-tcode = 'IW31' or
     sy-tcode = 'IW32' or
     sy-tcode = 'IW34' or
     sy-tcode = 'IW21' or
**<<<------"186781 - NMS - INI------>>>
   ( sy-tcode is initial    and
     sy-cprog eq 'SAPMHTTP' and
   ( trtyp    eq 'H'        or   "Criar
     trtyp    eq 'V' ) )    or   "Modificar
**<<<------"186781 - NMS - FIM------>>>
     sy-tcode = 'IW22' ) and lv_tcode is initial. "Não validar se a exit estiver sido chamada pela transação ZPM0026

  data: t_riwol type table of riwol,
        r_auart type range of auart.

  select * from tvarvc
    into table @data(t_tvarv)
    where name eq 'Z_TIPO_DOCUMENTO'
      and type eq 'S'.

  r_auart = value #( for s_tvarv in t_tvarv ( sign = s_tvarv-sign option = s_tvarv-opti low = s_tvarv-low high = s_tvarv-high ) ).

  if caufvd_imp-auart in r_auart.
    call function 'IWOL_GET_OBJECT_LIST_ALL'
      exporting
        i_aufnr        = caufvd_imp-aufnr
      tables
        iriwol         = t_riwol
      exceptions
        no_object_list = 1
        no_order       = 2
        others         = 3.

    delete t_riwol where dbknz eq 'D'.
    loop at t_riwol into data(wa_riwol).
      if wa_riwol-equnr ne caufvd_imp-equnr.
        message 'Nº de Equipamentos diferentes para a mesma ordem, favor ajustar' type 'E'.
      endif.
    endloop.

  endif.
endif.


* 144078 - DEVK9A1UDP - Obrigatoriedade de campos em ordem de manutenção - 04/07/24 - RSA
types: begin of types_afvgd.
         include structure afvgb.
types: end of types_afvgd.


data: ti_afvgd type standard table of types_afvgd,
      w_afvgd  type types_afvgd.

data: rg_auart type range of caufvd-auart,
      rg_iwerk type range of caufvd-iwerk.


constants: c_zpmst_tipo_ordem(16)          type c value 'ZPMST_TIPO_ORDEM',
           c_zpmst_centro_planejamento(25) type c value 'ZPMST_CENTRO_PLANEJAMENTO'.


field-symbols: <fs_afvgd>  type any.


assign ('(SAPLCOBP)AFVG_BT[]') to <fs_afvgd>.
check <fs_afvgd> is assigned.
ti_afvgd = <fs_afvgd>.


** 144078 - DEVK9A1UDP - Obrigatoriedade de campos em ordem de manutenção - 04/07/24 - RSA
if ( sy-tcode = 'IW31' or
     sy-tcode = 'IW32' or
**<<<------"186781 - NMS - INI------>>>
   ( sy-tcode is initial    and
     sy-cprog eq 'SAPMHTTP' and
   ( trtyp    eq 'H'        or   "Criar
     trtyp    eq 'V' ) )    or   "Modificar
**<<<------"186781 - NMS - FIM------>>>
     sy-tcode = 'IW34' ).

  " Seleciona tipos de ordens válidos
  select * from tvarvc
  into table @data(it_tp_ordens)
  where name eq @c_zpmst_tipo_ordem.
  if sy-subrc eq 0.
    rg_auart = value #( for i in it_tp_ordens ( sign = 'I' option = 'EQ' low = i-low ) ).
  endif.

** 144078 - DEVK9A1UDP - Obrigatoriedade de campos em ordem de manutenção - 04/07/24 - RSA
  " Seleciona tipos de planejamentos válidos
  select sign as sign,
  opti as option,
  low  as low
  from tvarvc
  into table @data(it_centro)
  where name eq @c_zpmst_centro_planejamento.
  if sy-subrc eq 0.
    rg_iwerk = value #( for t in it_centro ( sign = 'I' option = 'EQ' low = t-low ) ).
  endif.

  if caufvd_imp-auart in rg_auart and caufvd_imp-iwerk in rg_iwerk.
    loop at ti_afvgd into w_afvgd.
      if w_afvgd-phflg eq abap_false. "PM - Ajuste validação para operação PM04 #152661 / AOENNING
        if not w_afvgd-ltxa1 is initial and w_afvgd-arbei eq space.
          message 'Campo Trabalho em operações deve ser preenchido' type 'E'.
        endif.
      endif.
    endloop.
  endif.
endif.
** 144078 - DEVK9A1UDP - Obrigatoriedade de campos em ordem de manutenção - 04/07/24 - RSA

*** US #170302 - MMSILVA - 24.06.2025 - Ini ***
if sy-tcode = 'IW31' or
   sy-tcode = 'IW32' or
**<<<------"186781 - NMS - INI------>>>
   ( sy-tcode is initial    and
     sy-cprog eq 'SAPMHTTP' and
   ( trtyp    eq 'H'        or   "Criar
     trtyp    eq 'V' ) )    or   "Modificar
**<<<------"186781 - NMS - FIM------>>>
   sy-tcode = 'IW34'.

  select single * from zpmt0064
    into @data(ls_zpmt0064)
    where centro = @caufvd_imp-werks
    and   tipo   = @caufvd_imp-auart.

  if sy-subrc is initial.

    "BUG IMPEDITIVO - 186327 - Verificar status da ordem - RGA
    call function 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
      exporting                              "#EC CI_USAGE_OK[2669857] ->  lista de obj não foi utilizada.
        number    = caufvd_imp-aufnr
      importing
        es_header = esheader
      tables
        return    = it_return
        et_olist  = it_olist
      exceptions
        others    = 01.
    if esheader-sys_status(4) = 'ABER'.
      if ls_zpmt0064-obrig is not initial and caufvd_imp-user4 is initial.
        message 'Obrigatório o preenchimento do Valor de Custo.' type 'S' display like 'E'.
        raise no_changes_allowed.
      endif.

    endif.

    "BUG IMPEDITIVO - 186327 - Verificar status da ordem - RGA - FIM

  endif.

endif.
*** US #170302 - MMSILVA - 24.06.2025 - Fim ***

*//=======================Comentado 04/09/2025 para subir as outras demandas homologadas / AOENNING.
*"153373 - CS2024000881 Inclusão de ID COUPA do contrato em RC PM - RGA
*if sy-tcode eq 'IW32'.
*
*  loop at it_resb into wa_resb.
*
*    if wa_resb-idnlf is not initial.
*
*      select count(*)
*        from zmmt0177
*        where contract_id = wa_resb-idnlf
*         and  item_number = wa_resb-matnr.
*      if sy-subrc eq 0.
*
*        data: lt_items         type table of bapiebanv,
*              lt_items_details type table of  bapieban,
*              ls_items         type bapiebanv,
*              lt_items_old     type table of bapiebanv,
*              ls_items_old     like line of lt_items_old,
*              lt_ret           type table of bapireturn.
*
**        DATA: ITEMS_old TYPE TABLE OF bapieban.
**
*        data(_banfn) = wa_resb-banfnr.
*
*        call function 'BAPI_REQUISITION_GETDETAIL'
*          exporting
*            number             = _banfn
*            account_assignment = 'X'
*          tables
*            requisition_items  = lt_items_details.
*
*
*        loop at lt_items_details into data(ls_details).
*
*          move-corresponding ls_details to ls_items_old.
*
*          append ls_items_old to lt_items_old.
*          append ls_items_old to lt_items.
*
*        endloop.
*
*        "dados atualizados do contrato COUPA
*        loop at lt_items assigning field-symbol(<fs_items>).
*          <fs_items>-preq_item = wa_resb-banfpo.
*          <fs_items>-agreement = wa_resb-idnlf.
*          <fs_items>-agmt_item = wa_resb-banfpo.
*        endloop.
*
*        call function 'BAPI_REQUISITION_CHANGE'
*          exporting
*            number                = _banfn
*          tables
*            requisition_items_old = lt_items_old
*            requisition_items_new = lt_items
*            return                = lt_ret.
*        " Check for errors
*        read table lt_ret with key type = 'E' transporting no fields.
*        if sy-subrc = 0.
*          message 'Erro ao atualizar Requisição de Compra' type 'E'.
*        else.
*          call function 'BAPI_TRANSACTION_COMMIT'.
*        endif.
*
*      else.
*
*        message 'Erro ao atualizar Requisição de Compra' type 'E' display like 'E'.
*        raise no_changes_allowed.
*      endif.
*
*    endif.
*
*  endloop.
*
*endif.
"153373 - CS2024000881 Inclusão de ID COUPA do contrato em RC PM - RGA - fim
