*&---------------------------------------------------------------------*
*&  Include           ZFI0029_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
form SELECIONA_DADOS.
  data: R_UTILS       type ref to LCL_UTILS,
        RETURN_STATUS type C, "Status 'X' se sucesso.
        CONT          type C.

  create object R_UTILS.

  if ( S_BUDAT is not initial ).
    select *
      from BKPF
      into table GT_BKPF
     where BUKRS in S_BUKRS
       and BELNR in S_BELNR
       and BUDAT in S_BUDAT
       and BLART in S_BLART
       and GJAHR = S_BUDAT-LOW(4).
  else.
    select *
      from BKPF
      into table GT_BKPF
     where BUKRS in S_BUKRS
       and BELNR in S_BELNR
       and BLART in S_BLART.
  endif.

  if ( GT_BKPF is not initial ).
* ---> S4 Migration - 15/06/2023 - MA
*    SELECT *
*      FROM BSEG
*      INTO TABLE GT_BSEG
*   FOR ALL ENTRIES IN GT_BKPF
*     WHERE BUKRS = GT_BKPF-BUKRS
*       AND GJAHR = GT_BKPF-GJAHR
*       AND BELNR = GT_BKPF-BELNR
*       AND HKONT IN S_HKONT.

    data LT_FIELDS type FAGL_T_FIELD.
    data: LT_BSEG type table of BSEG,
          T_BSEG  type table of BSEG.

    call function 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      exporting
        IT_FOR_ALL_ENTRIES = GT_BKPF
        I_WHERE_CLAUSE     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR AND |
*       IT_FIELDLIST       = LT_FIELDS
      importing
        ET_BSEG            = LT_BSEG
      exceptions
        NOT_FOUND          = 1.

    delete LT_BSEG where HKONT not in S_HKONT.

    if SY-SUBRC = 0 and LINES( LT_BSEG ) > 0.
      append lines of LT_BSEG to GT_BSEG.
      SY-DBCNT = LINES( LT_BSEG ).
    else.
      SY-SUBRC = 4.
      SY-DBCNT = 0.
    endif.
*<--- S4 Migration - 15/06/2023 - MA
    loop at GT_BSEG into WL_BSEG.
      clear: WL_SAIDA-ESTILO, GT_ESTILO[],
             WL_ZIB_CONTABIL, RETURN_STATUS, WL_CE4MAGI_ACCT.

      read table GT_BKPF into WL_BKPF with key BUKRS = WL_BSEG-BUKRS
                                               BELNR = WL_BSEG-BELNR
                                               GJAHR = WL_BSEG-GJAHR.

      check ( WL_BKPF-AWKEY(3) ne 'ACD' ).
      CONT = SPACE.

*     ----- Concatena OBJKEY para ir na ZIB selecionar os registros correspondentes ------.
      R_UTILS->CONCATENAR_OBJKEY( exporting
                                  I_BELNR  = WL_BSEG-BELNR
                                  I_GJAHR  = WL_BSEG-GJAHR
                                  I_ESTOR  = C_X
                                  I_SEQ    = CONT
                                  importing
                                  E_OBJKEY = AT_OBJ_KEY ).

      select single *
        from ZIB_CONTABIL
        into WL_ZIB_CONTABIL
       where OBJ_KEY = AT_OBJ_KEY.

      if ( SY-SUBRC is initial ).

        WL_SAIDA-STATUS = ICON_YELLOW_LIGHT.

*       Verifica se o registro foi estornado, e também se foi criado algum outro após
*       o estorno.

*       Obs: Deixei previsto até 3 estornos.

        if ( WL_ZIB_CONTABIL-RG_ATUALIZADO = 'E' ).
          CONT = CONT + 1.

          while ( CONT <= 2 ).
            R_UTILS->CONCATENAR_OBJKEY( exporting
                                        I_BELNR  = WL_BSEG-BELNR
                                        I_GJAHR  = WL_BSEG-GJAHR
                                        I_ESTOR  = C_X
                                        I_SEQ    = CONT
                                        importing
                                        E_OBJKEY = AT_OBJ_KEY ).

            select single *
              from ZIB_CONTABIL
              into WL_ZIB_CONTABIL
             where OBJ_KEY = AT_OBJ_KEY.

            if ( SY-SUBRC is initial ).
              if ( WL_ZIB_CONTABIL-RG_ATUALIZADO eq 'E' ).
                CONT = CONT + 1.
              else.
                RETURN_STATUS = 'X'.
              endif .
            else.
              WL_SAIDA-STATUS = ICON_STORNO.
              RETURN_STATUS = 'X'.
            endif.

            check RETURN_STATUS = 'X'.
            exit.
          endwhile.

        elseif ( WL_ZIB_CONTABIL-RG_ATUALIZADO = 'F' ).
          WL_SAIDA-STATUS = ICON_CHECKED.
        endif.

      else.
        WL_SAIDA-STATUS = ICON_LIGHT_OUT.
      endif.

      if ( WL_SAIDA-STATUS ne ICON_CHECKED ).

*     ----- Verifica se o registro foi processado com sucesso -----.
        select single *
          from ZIB_CONTABIL_CHV
          into WL_ZIB_CONTABIL_CHV
         where OBJ_KEY = AT_OBJ_KEY.

        if ( SY-SUBRC is initial ).
          WL_SAIDA-STATUS    = ICON_GREEN_LIGHT.
        else.

*     ----- Verifica se o registro foi processado e ocorreu erros -----.
          select single *
            from ZIB_CONTABIL_ERR
            into WL_ZIB_CONTABIL_ERR
           where OBJ_KEY = AT_OBJ_KEY.
          if ( SY-SUBRC is initial ).
            WL_SAIDA-STATUS = ICON_RED_LIGHT.
          endif.
        endif.

      endif.

*     ----- Seleciona Material -----
*      IF ( WL_BSEG-KOART = 'M' ).
*        WL_SAIDA-MATNR = WL_BSEG-MATNR.
*      ELSE.
*        SELECT SINGLE *
*          FROM CE4MAGI_ACCT
*          INTO WL_CE4MAGI_ACCT
*         WHERE PAOBJNR = WL_BSEG-PAOBJNR.
*
*        WL_SAIDA-MATNR = WL_CE4MAGI_ACCT-ARTNR.
*      ENDIF.

      if ( WL_BSEG-MATNR is initial ).

        select single *
          from CE4MAGI_ACCT
          into WL_CE4MAGI_ACCT
         where PAOBJNR = WL_BSEG-PAOBJNR.

        WL_SAIDA-MATNR = WL_CE4MAGI_ACCT-ARTNR.
      else.
        WL_SAIDA-MATNR = WL_BSEG-MATNR.
      endif.
*      --

      clear: WL_RSEG, WL_MSEG.

      if WL_BKPF-BLART = 'RE'
      or WL_BKPF-BLART = 'ZG'.
*      or wl_bkpf-blart = 'WE'.

        select single *
          from RSEG
          into WL_RSEG
         where BELNR = WL_BKPF-AWKEY(10)
           and GJAHR = WL_BKPF-GJAHR.

        WL_SAIDA-EBELN = WL_RSEG-EBELN.
        WL_SAIDA-EBELP = WL_RSEG-EBELP.

      elseif WL_BKPF-BLART eq 'WE'.

        select single *
          from MSEG
          into WL_MSEG
         where MBLNR = WL_BKPF-AWKEY(10)
           and GJAHR = WL_BKPF-GJAHR.

        WL_SAIDA-EBELN = WL_MSEG-EBELN.
        WL_SAIDA-EBELP = WL_MSEG-EBELP.
      endif.



      if ( not WL_ZIB_CONTABIL is initial ).

        WL_SAIDA-TX_AJUSTE = WL_ZIB_CONTABIL-XREF3.
        WL_SAIDA-OBJKEY    = AT_OBJ_KEY.

*       Desabilita os campos do registro, caso não esteja estornado.
        if ( WL_ZIB_CONTABIL-RG_ATUALIZADO ne 'E' ).

          R_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = 'CHECK'
                                         STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).

          R_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = 'TX_AJUSTE'
                                         STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).

          R_UTILS->Z_STYLE_DISABLE_EDIT( FIELDNAME = 'VLR_AJUS'
                                         STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED ).

          insert lines of GT_ESTILO into table WL_SAIDA-ESTILO.
        endif.

      else.
        if WL_BKPF-WAERS = C_BRL.
          R_UTILS->SELECIONA_MOEDA( exporting
                                    I_MOEDA1 = C_BRL
                                    I_MOEDA2 = C_USD
                                    I_DATA   = WL_BKPF-BUDAT
                                    importing
                                    E_TAXA   = WL_SAIDA-TX_AJUSTE ).
        else.
          R_UTILS->SELECIONA_MOEDA( exporting
                                    I_MOEDA1 = C_USD
                                    I_MOEDA2 = C_BRL
                                    I_DATA   = WL_BKPF-BUDAT
                                    importing
                                    E_TAXA   = WL_SAIDA-TX_AJUSTE ).
        endif.
      endif.

      if ( WL_BSEG-ZFBDT = SPACE ).
        WL_SAIDA-ZFBDT = '00000000'.
      else.
        WL_SAIDA-ZFBDT = WL_BSEG-ZFBDT.
      endif.

      WL_SAIDA-BUKRS     = WL_BSEG-BUKRS.
      WL_SAIDA-BELNR     = WL_BKPF-BELNR.
      WL_SAIDA-GJAHR     = WL_BKPF-GJAHR.
      WL_SAIDA-BUDAT     = WL_BKPF-BUDAT.
      WL_SAIDA-HKONT     = WL_BSEG-HKONT.
      WL_SAIDA-BSCHL     = WL_BSEG-BSCHL.
      WL_SAIDA-SHKZG     = WL_BSEG-SHKZG.
      WL_SAIDA-GSBER     = WL_BSEG-GSBER.
      WL_SAIDA-KUNNR     = WL_BSEG-KUNNR.
      WL_SAIDA-LIFNR     = WL_BSEG-LIFNR.
      WL_SAIDA-KOSTL     = WL_BSEG-KOSTL.
      WL_SAIDA-AUFNR     = WL_BSEG-AUFNR.
      WL_SAIDA-BUZID     = WL_BSEG-BUZID.
      WL_SAIDA-KOART     = WL_BSEG-KOART.
      WL_SAIDA-WAERS     = WL_BKPF-WAERS.
      WL_SAIDA-ZLSPR     = WL_BSEG-ZLSPR.
      WL_SAIDA-ZLSCH     = WL_BSEG-ZLSCH.
      WL_SAIDA-PRCTR     = WL_BSEG-PRCTR.
      WL_SAIDA-XBLNR     = WL_BKPF-XBLNR.
      WL_SAIDA-BLART     = WL_BKPF-BLART.
      WL_SAIDA-STBLG     = WL_BKPF-STBLG.
*      WL_SAIDA-ZFBDT     = WL_BSEG-ZFBDT.
      WL_SAIDA-KIDNO     = WL_BSEG-KIDNO.
      WL_SAIDA-TCODE     = WL_BKPF-TCODE.
      WL_SAIDA-UMSKZ     = WL_BSEG-UMSKZ.
      WL_SAIDA-BLDAT     = WL_BKPF-BLDAT.
      WL_SAIDA-SGTXT     = WL_BSEG-SGTXT.
      WL_SAIDA-ZUONR     = WL_BSEG-ZUONR.

      if ( WL_SAIDA-SHKZG  = 'H' ).
        WL_SAIDA-DMBTR_BRL = ( WL_BSEG-DMBTR * -1 ).
        WL_SAIDA-DMBTR_USD = ( WL_BSEG-DMBE2 * -1 ).
      else.
        WL_SAIDA-DMBTR_BRL = WL_BSEG-DMBTR.
        WL_SAIDA-DMBTR_USD = WL_BSEG-DMBE2.
      endif.

      try.
          WL_SAIDA-TX_CAMBIO = ( WL_BSEG-DMBTR / WL_BSEG-DMBE2 ).
        catch CX_SY_ZERODIVIDE.
      endtry.

      try.
          WL_SAIDA-VLR_CALC  = ( WL_SAIDA-DMBTR_BRL / WL_SAIDA-TX_AJUSTE ).
        catch CX_SY_ZERODIVIDE.
      endtry.

      WL_SAIDA-VLR_AJUS  = ( WL_SAIDA-VLR_CALC - WL_SAIDA-DMBTR_USD ).

      append WL_SAIDA to GT_SAIDA.
      clear WL_SAIDA.
    endloop.

    sort GT_SAIDA by BELNR.

  else.
    message text-E01 type 'I' display like 'E'.
  endif.
endform.                    "SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
form ALV_PREENCHE_CAT       using: P_CAMPO         type C
                                   P_DESC          type C
                                   P_TAM           type C
                                   P_HOT           type C
                                   P_ZERO          type C
                                   P_SUM           type C
                                   P_ICON          type C
                                   P_EDIT          type C
                                   P_CHECK         type C
                                   P_HOTSPOT       type C
                                   P_REF_TABNAME   like DD02D-TABNAME
                                   P_REF_FIELDNAME like DD03D-FIELDNAME
                                   P_TABNAME       like DD02D-TABNAME
                                   P_MASK  type C.

  WL_FCAT-FIELDNAME  = P_CAMPO.
  WL_FCAT-SCRTEXT_L  = P_DESC.
  WL_FCAT-SCRTEXT_M  = P_DESC.
  WL_FCAT-SCRTEXT_S  = P_DESC.
  WL_FCAT-HOTSPOT    = P_HOT.
  WL_FCAT-NO_ZERO    = P_ZERO.
  WL_FCAT-OUTPUTLEN  = P_TAM.
  WL_FCAT-ICON       = P_ICON.
  WL_FCAT-CHECKBOX   = P_CHECK.
  WL_FCAT-EDIT       = P_EDIT.
  WL_FCAT-HOTSPOT    = P_HOTSPOT.
  WL_FCAT-REF_TABLE  = P_REF_TABNAME.
  WL_FCAT-REF_FIELD  = P_REF_FIELDNAME.
  WL_FCAT-TABNAME    = P_REF_TABNAME.
  WL_FCAT-EDIT_MASK = P_MASK.

  append WL_FCAT to GT_FCAT.
endform.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WL_EXC_BUTTON  text
*----------------------------------------------------------------------*
form EXCLUDE_TB_FUNCTIONS changing WL_EXC_BUTTON type UI_FUNCTIONS.
  append CL_GUI_ALV_GRID=>MC_FC_REFRESH           to WL_EXC_BUTTON.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    to WL_EXC_BUTTON.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    to WL_EXC_BUTTON.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    to WL_EXC_BUTTON.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO WL_EXC_BUTTON.
  append CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      to WL_EXC_BUTTON.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO WL_EXC_BUTTON.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO WL_EXC_BUTTON.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO WL_EXC_BUTTON.
*  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO WL_EXC_BUTTON.
  append CL_GUI_ALV_GRID=>MC_FC_CHECK             to WL_EXC_BUTTON.
endform.                    "EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  REGISTER_F4_FOR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form REGISTER_F4_FOR_FIELDS.
  data WL_F4 type LVC_T_F4 with header line.

  WL_F4-FIELDNAME = 'CHECK'.
  WL_F4-REGISTER  = 'X' .
  WL_F4-GETBEFORE = 'X' .
  append WL_F4.

  WL_F4-FIELDNAME = 'TX_AJUSTE'.
  WL_F4-REGISTER  = 'X' .
  WL_F4-GETBEFORE = 'X' .
  append WL_F4.

  WL_F4-FIELDNAME = 'VLR_AJUS'.
  WL_F4-REGISTER  = 'X' .
  WL_F4-GETBEFORE = 'X' .
  append WL_F4.

  call method OBJ_ALV_0100->REGISTER_F4_FOR_FIELDS
    exporting
      IT_F4 = WL_F4[].

endform.                    "REGISTER_F4_FOR_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_START    text
*      -->L_NAME     text
*      -->L_VALUE    text
*----------------------------------------------------------------------*
form F_PREENCHER_DYNPRO using L_START type C
                              L_NAME  type C
                              L_VALUE.

  move L_START to WL_BDC-DYNBEGIN.
  if L_START = 'X'.
    move:
        L_NAME  to WL_BDC-PROGRAM,
        L_VALUE to WL_BDC-DYNPRO.
  else.
    move:
        L_NAME  to WL_BDC-FNAM,
        L_VALUE to WL_BDC-FVAL.
  endif.

  append WL_BDC to GT_BDC.
  clear WL_BDC.
endform.                    "F_PREENCHER_DYNPRO
