"Name: \PR:SAPLMEREQ\TY:LCL_REQ_ITEM\ME:PREPARE_POST\SE:BEGIN\EI
ENHANCEMENT 0 ZLMEREQF05.
    DATA: T_ZMMT0004 TYPE STANDARD TABLE OF ZMMT0004 ,
        T_ZMMT0004_AUX TYPE STANDARD TABLE OF ZMMT0004 ,
        T_ESLL TYPE STANDARD TABLE OF ESLL ,
        T_ESLL_AUX TYPE STANDARD TABLE OF ESLL ,
        WA_ZMMT0004 TYPE ZMMT0004 ,
        l_mereq TYPE mereq_item,
* ---> S4 Migration - 19/06/2023 - MA
*        w_messa(50),
        w_messa(80),
* <--- S4 Migration - 19/06/2023 - MA
        w_mat   type mara-matnr,
        WA_ESLL TYPE ESLL.

  DATA: T_IX_ESLL TYPE STANDARD TABLE OF ZIX_ESLL,
        WA_IX_ESLL TYPE ZIX_ESLL.

  DATA: L_SRVPOS TYPE ESLL-SRVPOS,
        L_SUB_PACKNO TYPE ESLL-SUB_PACKNO,
        L_SRVPOS_RCC TYPE ESLL-SRVPOS,
        I_LINEID TYPE SETLEAF-LINEID,
        I_LNAME TYPE RWSLINE-LNAME,
        vg_asnum type zmmt0061-asnum.

DATA: I_VALFROM TYPE SETLEAF-VALFROM,
      I_QTD     TYPE I.

FIELD-SYMBOLS: <fs_esll> TYPE TABLE.
CONSTANTS: c_esll(28) VALUE '(SAPLMLSP)IX_ESLL[]'.
ASSIGN (c_esll) TO <fs_esll>.

IF <fs_esll> IS ASSIGNED.
  t_ix_esll[] = <fs_esll>.
    READ TABLE T_IX_ESLL  INTO WA_IX_ESLL
             WITH KEY MSUPDAP-PACKNO = ME->MY_STATE->ITEM-PACKNO
                      MSUPDAP-PACKAGE = 'X'.
    IF SY-SUBRC EQ 0.
      READ TABLE T_IX_ESLL  INTO WA_IX_ESLL
           WITH KEY MSUPDAP-PACKNO = WA_IX_ESLL-MSUPDAP-SUB_PACKNO.
                    "MSUPDAP-MATKL  = ME->MY_STATE->ITEM-MATKL.
      IF SY-SUBRC EQ 0.
        L_SRVPOS     = WA_IX_ESLL-MSUPDAP-SRVPOS.
        L_SRVPOS_RCC = WA_IX_ESLL-MSUPDAP-SRVPOS.
      ENDIF.
    ENDIF.
ENDIF.


IF SY-TCODE EQ 'ME51N' or
   SY-TCODE EQ 'ME53N' or
   SY-TCODE EQ 'ZMM0072' or
   SY-CPROG eq 'ZMMR047'.

    IF ( ( ME->MY_STATE->ITEM-BSART EQ 'RCC' ) AND  ( ME->MY_STATE->ITEM-PSTYP EQ '9' ) ) or
       SY-TCODE EQ 'ZMM0072' or SY-CPROG eq 'ZMMR047'.
        IF L_SRVPOS_RCC IS NOT INITIAL.
          I_VALFROM    = L_SRVPOS_RCC.
          CALL FUNCTION 'STRING_LENGTH'
            EXPORTING
              STRING = I_VALFROM
            IMPORTING
              LENGTH = I_QTD.

          I_QTD = 18 - I_QTD.

          DO I_QTD TIMES.
            CONCATENATE '0' I_VALFROM INTO I_VALFROM.
          ENDDO.

          SELECT SINGLE LINEID INTO I_LINEID
            FROM SETLEAF
           WHERE SETNAME EQ 'ESTR_REQ_RCC'
             AND VALFROM EQ I_VALFROM.

          IF SY-SUBRC EQ 0.

            SELECT SINGLE LNAME INTO I_LNAME
              FROM RWSLINE
             WHERE SETNAME EQ 'ESTR_REQ_RCC'
               AND LINEID  EQ I_LINEID.

            IF SY-SUBRC EQ 0.
              ME->MY_STATE->ITEM-FRGST = I_LNAME.
            ENDIF.

          ENDIF.

        ENDIF.

    endif.


" Validação de Comprador/Regulariza - Inicio
  case me->my_state->item-BSART.
    when: 'REG'.
*          if ( ME->MY_STATE->ITEM-EKGRP ne 'C99').
*              MESSAGE e000(z01) WITH 'Comprador regulariza somente '
*                                     'permitido C99'.
*           endif.
    when: 'ZMAR'.
          if ( ME->MY_STATE->ITEM-EKGRP ne 'C99').
               MESSAGE e000(z01) WITH 'Comprador regulariza somente '
                                      'permitido C99'.
           endif.
    when: 'ZOUT'.
*          if ( not  'C87,C94,C95,C96,C97,C98,C86,C92,D01,D02,D03,D04,D05,C85,C88,C89,C90,C91' cs ME->MY_STATE->ITEM-EKGRP ).
*            message 'Compradores permitidos C87,C94,C95,C96,C97,C98,C86,C92,D01,D02,D03,D04,D05,C85,C88,C89,C90,C91' type 'E'.
*           endif.
   endcase.


" Validação de Comprador/Regulariza - Fim

" Parametros tipo pedido
data:
      wa_ZMMT0061 type          ZMMT0061,
      WA_T161     type          T161T,
      W_msg(60),
      fg_matkl(1),
      fg_matnr(1),
      fg_ekgrp(1).

if sy-ucomm = 'MESAVE'.

*      IF ( 'NB_REG_ZOUT' CS me->my_state->item-BSART ).
*          if me->my_state->item-BSART = 'NB'.
*              MESSAGE E398(00) WITH 'O tipo de R.C.'
*                                    me->my_state->item-BSART
*                                   ' está indisponível,  utilizar R.C. RCS .'.
*          else.
*             MESSAGE E398(00) WITH 'O tipo de R.C.'
*                                    me->my_state->item-BSART
*                                   ' está indisponível,  utilizar pedido PCE.'.
*          endif.
*      ENDIF.

  clear wa_ZMMT0061.
  SELECT SINGLE *
    from ZMMT0061
    into wa_ZMMT0061
    WHERE BSTYP = 'B'
    and   bsart eq me->my_state->item-BSART
    and   ekgrp eq me->my_state->item-ekgrp
    and   MATKL eq ''
    and   matnr eq ''
    and   ASNUM eq ''.

   IF sy-subrc ne 0.
     SELECT SINGLE *
      from ZMMT0061
      into wa_ZMMT0061
      WHERE BSTYP = 'B'
      and   ekgrp eq me->my_state->item-ekgrp
      and   MATKL eq ''
      and   matnr eq ''
      and   ASNUM eq ''.
      IF sy-subrc eq 0. "
          select SINGLE *
                  from T161T
                  into WA_T161
                  where BSTYP = 'B'
                  and   SPRAS = sy-langu
                  and   bsart = wa_ZMMT0061-BSART.
          MESSAGE E398(00) WITH 'Comprador:'
                        me->my_state->item-ekgrp
                        ' permitido apenas para R.C. do tipo '
                        WA_T161-BATXT .
      endif.
    endif.

    if me->my_state->item-MATKL is not INITIAL.
      SELECT SINGLE *
        from ZMMT0061
        into wa_ZMMT0061
        WHERE BSTYP = 'B'
        and   bsart eq me->my_state->item-BSART
        and   MATKL eq me->my_state->item-MATKL.

       IF sy-subrc ne 0. " Se encontrou Permitido Grupo para este tipo de Req.
          SELECT SINGLE * " Se encontrou em outro tipo de Req NÃO Permitido
              from ZMMT0061
              into wa_ZMMT0061
             WHERE BSTYP = 'B'
             and   MATKL eq me->my_state->item-MATKL.
          IF sy-subrc = 0.
               select SINGLE *
                from T161T
                into WA_T161
                where BSTYP = 'B'
                and   SPRAS = sy-langu
                and   bsart = wa_ZMMT0061-BSART.
             MESSAGE E398(00) WITH 'Grupo de Material:'
                           me->my_state->item-MATKL
                           ' permitido apenas para R.C. do tipo '
                           WA_T161-BATXT .
          ENDIF.

       ENDIF.
      endif.

      if me->my_state->item-matnr is not INITIAL.
      SELECT SINGLE *
        from ZMMT0061
        into wa_ZMMT0061
        WHERE BSTYP = 'B'
        and   bsart eq me->my_state->item-BSART
        and   matnr eq me->my_state->item-matnr
        and   matnr gt 0.

       IF sy-subrc ne 0. " Se encontrou Permitido Grupo para este tipo de Req.
          SELECT SINGLE * " Se encontrou em outro tipo de Req NÃO Permitido
              from ZMMT0061
              into wa_ZMMT0061
              WHERE BSTYP = 'B'
              and   matnr eq me->my_state->item-matnr.
          IF sy-subrc = 0.
             select SINGLE *
                from T161T
                into WA_T161
                where BSTYP = 'B'
                and   SPRAS = sy-langu
                and   bsart = wa_ZMMT0061-BSART.
            MESSAGE E398(00) WITH 'Material:'
                                  me->my_state->item-matnr
                                  ' permitido apenas para R.C. do tipo '
                                  WA_T161-BATXT .
          ENDIF.
        ENDIF.
      elseif L_SRVPOS_RCC is not INITIAL. "03.12.2015 ALRS
        SELECT SINGLE *
          from ZMMT0061
          into wa_ZMMT0061
          WHERE BSTYP = 'B'
          and   bsart        eq me->my_state->item-BSART
          and   ASNUM        eq L_SRVPOS_RCC.
         IF sy-subrc ne 0. " Se encontrou Permitido Grupo para este tipo de Req.
            SELECT SINGLE * " Se encontrou em outro tipo de Req NÃO Permitido
                from ZMMT0061
                into wa_ZMMT0061
                WHERE BSTYP = 'B'
                and   ASNUM        eq L_SRVPOS_RCC.
            IF sy-subrc = 0.
               select SINGLE *
                  from T161T
                  into WA_T161
                  where BSTYP = 'B'
                  and   SPRAS = sy-langu
                  and   bsart = wa_ZMMT0061-BSART.
              MESSAGE E398(00) WITH 'Servico:'
                                    L_SRVPOS_RCC
                                    ' permitido apenas para R.C. do tipo '
                                    WA_T161-BATXT .
            ENDIF.
          ELSEIF wa_ZMMT0061-ekgrp ne me->my_state->item-ekgrp.
            MESSAGE I398(00) WITH 'Comprador:'
                                  wa_ZMMT0061-ekgrp
                                  ' é o correto para este tipo de R.C./Material'.
            me->my_state->item-ekgrp = wa_ZMMT0061-ekgrp.
          endif.
     " retirado em 01/09/2015
     "SOMENTE PARA ZOUT
*     IF me->my_state->item-BSART = 'ZOUT'.
*       SELECT SINGLE *
*         from ZMMT0061
*         into wa_ZMMT0061
*         where bsart eq me->my_state->item-BSART
*         and   matnr eq me->my_state->item-matnr.
*
*       IF sy-subrc ne 0. " .
*         select SINGLE *
*                from T161T
*                into WA_T161
*                where BSTYP = 'B'
*                and   SPRAS = sy-langu
*                and   bsart = wa_ZMMT0061-BSART.
*           MESSAGE E398(00) WITH 'Material:'
*                                  me->my_state->item-matnr
*                                  'NÃO permitido para R.C. do tipo '
*                                  WA_T161-BATXT .
*        ENDIF.
*     ENDIF.

    endif.

    if my_state->item-ekgrp is not INITIAL and me->my_state->item-matnr is not INITIAL.
      clear wa_ZMMT0061.
      clear wa_ZMMT0061.
      SELECT SINGLE *
        from ZMMT0061
        into wa_ZMMT0061
        WHERE BSTYP = 'B'
        and   bsart eq me->my_state->item-BSART
        and   matnr eq me->my_state->item-matnr
        and   ekgrp eq me->my_state->item-ekgrp.

       IF sy-subrc ne 0. " Se encontrou Permitido Grupo para este tipo de Req.
          SELECT SINGLE * " Se encontrou em outro tipo de Req NÃO Permitido
              from ZMMT0061
              into wa_ZMMT0061
              WHERE BSTYP = 'B'
              and   bsart eq me->my_state->item-BSART
              and   matnr eq me->my_state->item-matnr
              and   matnr gt 0.
          IF sy-subrc = 0.
            MESSAGE I398(00) WITH 'Comprador:'
                                  wa_ZMMT0061-ekgrp
                                  ' é o correto para este tipo de R.C./Material'.
            me->my_state->item-ekgrp = wa_ZMMT0061-ekgrp.
          ENDIF.

       ENDIF.

      clear wa_ZMMT0061.
      SELECT SINGLE *
        from ZMMT0061
        into wa_ZMMT0061
        WHERE BSTYP = 'B'
        and   bsart eq me->my_state->item-BSART
        and   MATKL eq me->my_state->item-MATKL
        and   ekgrp eq me->my_state->item-ekgrp.

       IF sy-subrc ne 0. " Se encontrou Permitido Grupo para este tipo de Req.
          SELECT SINGLE * " Se encontrou em outro tipo de Req NÃO Permitido
              from ZMMT0061
              into wa_ZMMT0061
              WHERE BSTYP = 'B'
              and   bsart eq me->my_state->item-BSART
              and   MATKL eq me->my_state->item-MATKL.
          IF sy-subrc = 0.
            MESSAGE I398(00) WITH 'Comprador:'
                                  wa_ZMMT0061-ekgrp
                                  ' é o correto para este tipo de R.C./Grupo Material'.
            me->my_state->item-ekgrp = wa_ZMMT0061-ekgrp.
          ENDIF.

       ENDIF.
      endif.

endif.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = me->my_state->item-MATNR
      IMPORTING
        OUTPUT = w_mat.

CONCATENATE   'Ampliar material' w_mat  'p/ dep.' me->my_state->item-LGORT into  w_messa SEPARATED BY space.

if ( me->my_state->item-KNTTP is initial ).
    DATA: VLMATNR TYPE MARD-MATNR.

    SELECT SINGLE MATNR
      FROM MARD
      INTO VLMATNR
     WHERE WERKS EQ me->my_state->item-WERKS
       AND MATNR EQ me->my_state->item-MATNR
       AND LGORT EQ me->my_state->item-LGORT.

    IF SY-SUBRC NE 0.
       MESSAGE w_messa TYPE 'E'.
    ENDIF.
  ENDIF.
ENDIF.

* Conforme chamado 52840
IF SY-TCODE EQ 'ME52N'.
  IF    ( me->my_state->item-KNTTP is initial ).
  DATA: VLMATNR2 TYPE MARD-MATNR.

  SELECT SINGLE MATNR
    FROM MARD
    INTO VLMATNR2
   WHERE WERKS EQ me->my_state->item-WERKS
     AND MATNR EQ me->my_state->item-MATNR
     AND LGORT EQ me->my_state->item-LGORT.

    IF SY-SUBRC NE 0.
      MESSAGE w_messa TYPE 'E'.
    ENDIF.
  ENDIF.
ENDIF.

"ALRS 05.01.2015 - texto breve
IF me->my_state->item-matnr IS NOT INITIAL.
  SELECT SINGLE MAKTX
    INTO me->my_state->item-TXZ01
    FROM MAKT
    WHERE SPRAS = SY-LANGU
    AND   MATNR = me->my_state->item-matnr.
ENDIF.

refresh: t_zmmt0004.

  IF me->my_state->item-matnr EQ ''.

    if me->my_state->item-KONNR is not INITIAL.
           select  *
            from zmmt0004
            into table t_zmmt0004
            where matnr = '' and
                 werks = me->my_state->item-WERKS and
                 ebeln = me->my_state->item-KONNR and
                 kdatb le sy-datum and
                 kdate ge sy-datum.
    endif.

    IF t_zmmt0004[] IS INITIAL.
       select  *
         from zmmt0004
         into table t_zmmt0004
         where matnr = '' and
               werks = me->my_state->item-WERKS and
               kdatb le sy-datum and
               kdate ge sy-datum.
    endif.

    IF NOT t_zmmt0004[] IS INITIAL.
      select *
        from esll
        into table t_esll
        for all entries in t_zmmt0004
        where packno = t_zmmt0004-packno.

      IF NOT t_esll[] IS INITIAL.
        Select *
        from esll
        into table t_esll_aux
        for all entries in t_esll
        where packno = t_esll-sub_packno and
                    srvpos = l_srvpos.

        IF SY-SUBRC EQ 0.
          LOOP AT t_zmmt0004 INTO wa_zmmt0004.
            CLEAR wa_esll.
            READ TABLE t_esll WITH KEY packno = wa_zmmt0004-packno INTO wa_esll.
            IF NOT wa_esll IS INITIAL.
              READ TABLE t_esll_aux WITH KEY packno = wa_esll-sub_packno TRANSPORTING NO FIELDS.
              IF sy-subrc eq 0.
                APPEND wa_zmmt0004 TO t_zmmt0004_aux.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

    delete t_zmmt0004_aux where loekz = 'L'.
    IF not t_zmmt0004_aux[] is initial.
      sort: t_zmmt0004_aux by ebeln descending ebelp descending.

      read table t_zmmt0004_aux into wa_zmmt0004 index 1.
       me->my_state->item-KONNR =  wa_zmmt0004-EBELN.
       me->my_state->item-KTPNR =  wa_zmmt0004-EBELP.
       me->my_state->item-FLIEF =  wa_zmmt0004-LIFNR.
       me->my_state->item-EKORG =  wa_zmmt0004-EKORG.
       me->my_state->item-VRTYP = 'K'.

    ENDIF.

  ENDIF.

  ELSE.

    if me->my_state->item-KONNR is not INITIAL.
       select  *
      from zmmt0004
      into table t_zmmt0004
      where matnr = me->my_state->item-matnr and
            werks = me->my_state->item-werks and
            ebeln = me->my_state->item-KONNR and
            loekz = ''                       and
            KDATB le sy-datum                and
            kdate ge  sy-datum.
    endif.
    if sy-subrc ne 0 or me->my_state->item-KONNR is INITIAL.
       select  *
         from zmmt0004
         into table t_zmmt0004
         where matnr = me->my_state->item-matnr and
               werks = me->my_state->item-werks and
               KDATB le sy-datum                and
               kdate ge  sy-datum.
    endif.

    IF SY-SUBRC = 0.
      delete t_zmmt0004 where loekz = 'L'.
      IF not t_zmmt0004[] is initial.
        sort: t_zmmt0004 by ebeln descending ebelp descending.

        read table t_zmmt0004 into wa_zmmt0004 index 1.
         me->my_state->item-KONNR =  wa_zmmt0004-EBELN.
         me->my_state->item-KTPNR =  wa_zmmt0004-EBELP.
         me->my_state->item-FLIEF =  wa_zmmt0004-LIFNR.
         me->my_state->item-EKORG =  wa_zmmt0004-EKORG.
         me->my_state->item-VRTYP = 'K'.

      ENDIF.

    ENDIF.

  ENDIF.
ENDENHANCEMENT.
