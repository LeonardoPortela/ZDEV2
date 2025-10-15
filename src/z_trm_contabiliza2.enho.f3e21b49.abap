"Name: \TY:CL_DOCUMENT_TRAC\ME:COLLECT_ACCOUNTING_ITEMS\SE:END\EI
ENHANCEMENT 0 Z_TRM_CONTABILIZA2.
*
   DATA: l_rpzahl               TYPE tb_rpzahl_new,
         l_distributionflowtype TYPE tpm_dis_flowtype,
         deal_number            TYPE tb_rfha,
         l_bzbetr               TYPE vtbfhapo-bzbetr,
         l_refext               TYPE sy-datum,
         l_vencto               TYPE sy-datum,
         l_contemplado(1).

   SELECT SINGLE *
      FROM zfit187
      INTO @DATA(_zfit187)
      WHERE sfhazba = @accitem_d->accit-dist_flowtype+2(4).

   IF sy-subrc = 0.
     CLEAR l_contemplado.
     CALL METHOD pflow->get_flow_data
       IMPORTING
         ex_rpzahl = l_rpzahl.

     IF _zfit187-shkzg = 'S'.
       l_vencto = accitem_d->accit-valut.
     ELSE.
       l_vencto = accitem_c->accit-valut.
     ENDIF.

     CALL METHOD getu_attributes
       IMPORTING
         ex_deal_number = deal_number.

     SELECT SINGLE *
       FROM vtbfhazu
       INTO @DATA(_vtbfhazu)
        WHERE bukrs = @accitem_d->accit-bukrs
       AND   rfha   = @deal_number
       AND   nordext NE ''.

     IF sy-subrc = 0.
       DATA(_len) =  strlen( _vtbfhazu-nordext ).
       IF _vtbfhazu-nordext+2(1) EQ '.' AND  _vtbfhazu-nordext+5(1) EQ '.' AND  _len = 10.
         CONCATENATE  _vtbfhazu-nordext+6(4) _vtbfhazu-nordext+3(2)  _vtbfhazu-nordext+0(2) INTO l_refext.
         IF l_refext LE l_vencto.
           l_contemplado = 'X'.
         ENDIF.
       ENDIF.
     ENDIF.

     SELECT *
       INTO TABLE @DATA(_lt_vtbfhapo)
       FROM vtbfhapo
       WHERE bukrs = @accitem_d->accit-bukrs
       AND   rfha  = @deal_number
       AND   sfhazba IN ( '1110', '1111' ).

     IF sy-subrc = 0.
       LOOP AT _lt_vtbfhapo INTO DATA(_ls_vtbfhapo) WHERE dzterm LE l_vencto.
         l_contemplado = 'X'.
         EXIT.
       ENDLOOP.
**       IF _vtbfhapo-dzterm LE l_vencto.
**         l_contemplado = 'X'.
**       ENDIF.
     ENDIF.

     SELECT  *
       FROM zfit187
       INTO TABLE @DATA(t_zfit187)
       WHERE sfhazba     = @accitem_d->accit-dist_flowtype+2(4)
       AND   contemplado = @l_contemplado.

     LOOP AT t_zfit187 INTO _zfit187.
       IF _zfit187-shkzg = 'S'.
         accitem_d->accit-bschl = _zfit187-bschl.
         accitem_d->accit-umskz = _zfit187-umskz.
         accitem_d->accit-koart = 'K'.
         accitem_d->accit-lifnr = l_rpzahl.
         SELECT SINGLE *
           INTO @DATA(_lfb1)
           FROM lfb1
           WHERE lifnr = @l_rpzahl.
         IF sy-subrc = 0.
           accitem_d->accit-saknr = _lfb1-akont.
         ELSE.
           CLEAR accitem_d->accit-hkont.
         ENDIF.

         SELECT SINGLE *
             FROM t074
             INTO @DATA(_t074)
            WHERE koart = 'K'
            AND   umskz = @accitem_d->accit-umskz
            AND   hkont = @_lfb1-akont.
         IF sy-subrc = 0.
           accitem_d->accit-hkont = _t074-skont.
         ENDIF.
         "
       ELSE.
         accitem_c->accit-bschl = _zfit187-bschl.
         accitem_c->accit-umskz = _zfit187-umskz.
         accitem_c->accit-koart = 'K'.
         accitem_c->accit-lifnr = l_rpzahl.
         SELECT SINGLE *
           INTO _lfb1
           FROM lfb1
           WHERE lifnr = l_rpzahl.
         IF sy-subrc = 0.
           accitem_c->accit-saknr = _lfb1-akont.
         ELSE.
           CLEAR accitem_c->accit-hkont.
         ENDIF.

         SELECT SINGLE *
             FROM t074
             INTO _t074
            WHERE koart = 'K'
            AND   umskz = accitem_c->accit-umskz
            AND   hkont = _lfb1-akont.
         IF sy-subrc = 0.
           accitem_c->accit-hkont = _t074-skont.
         ELSE.
           accitem_c->accit-hkont = _lfb1-akont.
         ENDIF.
       ENDIF.
     ENDLOOP.
   ENDIF.

   DATA(lv_id) = me->get_deal_number( ).
   DATA(lv_empresa) = me->get_company_code( ).



   SELECT SINGLE zuond
     FROM vtbfha
     INTO @DATA(lv_contrato)
     WHERE bukrs = @lv_empresa
         AND rfha = @lv_id.
   IF sy-subrc = 0.
     accitem_c->accit-zuonr = lv_contrato.
     accitem_d->accit-zuonr = lv_contrato.
   ENDIF.


ENDENHANCEMENT.
