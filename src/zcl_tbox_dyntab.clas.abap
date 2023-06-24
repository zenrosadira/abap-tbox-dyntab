CLASS zcl_tbox_dyntab DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_field_value,
        field TYPE string,
        value TYPE string,
      END OF ts_field_value.
    TYPES tt_field_value TYPE TABLE OF ts_field_value WITH KEY field.
    TYPES:
      BEGIN OF ts_tab_field_value,
        index TYPE i,
        row   TYPE tt_field_value,
      END OF ts_tab_field_value.
    TYPES tt_tab_field_value TYPE TABLE OF ts_tab_field_value WITH KEY index.

    METHODS del_column
      IMPORTING column_name TYPE clike.

    METHODS add_column
      IMPORTING column_name TYPE clike
                !like       TYPE data                     OPTIONAL
                !type       TYPE string                   OPTIONAL
                type_ref    TYPE REF TO cl_abap_typedescr OPTIONAL.

    METHODS get_subtable
      IMPORTING !columns TYPE string_table
      RETURNING VALUE(r) TYPE REF TO data.

    METHODS get
      EXPORTING !table   TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    METHODS extend_catalog
      IMPORTING !tab TYPE ANY TABLE.

    METHODS extend_with_table
      IMPORTING !tab TYPE ANY TABLE.

    CLASS-METHODS create_by_transposition
      IMPORTING !tab         TYPE ANY TABLE
                column_field TYPE clike
                column_value TYPE clike
                prefix       TYPE clike OPTIONAL
      RETURNING VALUE(r)     TYPE REF TO zcl_tbox_dyntab.

    CLASS-METHODS create_from_table
      IMPORTING !tab     TYPE ANY TABLE
      RETURNING VALUE(r) TYPE REF TO zcl_tbox_dyntab.

    METHODS append_ref
      IMPORTING !row TYPE REF TO data.

    METHODS append
      IMPORTING !row TYPE data.

    METHODS append_lines
      IMPORTING !tab TYPE ANY TABLE.

    METHODS read_with_condition
      IMPORTING !condition TYPE string
      RETURNING VALUE(r)   TYPE REF TO data.

    METHODS read_with_keys
      IMPORTING with_keys TYPE tt_field_value
      RETURNING VALUE(r)  TYPE REF TO data.

    METHODS clear.

    CLASS-METHODS create_from_catalog
      IMPORTING catalog  TYPE abap_component_view_tab
      RETURNING VALUE(r) TYPE REF TO zcl_tbox_dyntab.

    METHODS read_index
      IMPORTING !index   TYPE i
      RETURNING VALUE(r) TYPE REF TO data.

    METHODS count
      RETURNING VALUE(r) TYPE i.

    CLASS-METHODS create_empty
      RETURNING VALUE(r) TYPE REF TO zcl_tbox_dyntab.

    METHODS constructor
      IMPORTING !tab TYPE REF TO data OPTIONAL.

    METHODS is_initial
      RETURNING VALUE(r) TYPE abap_bool.

    METHODS pick_column
      IMPORTING !column  TYPE clike
      RETURNING VALUE(r) TYPE REF TO data.

    METHODS change_column_type
      IMPORTING column_name TYPE clike
                !like       TYPE data                     OPTIONAL
                !type       TYPE string                   OPTIONAL
                type_ref    TYPE REF TO cl_abap_typedescr OPTIONAL
      RETURNING VALUE(r)    TYPE REF TO zcl_tbox_dyntab.

    CLASS-METHODS transpose_table
      IMPORTING i_data   TYPE ANY TABLE
      RETURNING VALUE(r) TYPE tt_tab_field_value.

    CLASS-METHODS detranspose_table
      IMPORTING i_tab_field_value TYPE tt_tab_field_value
      RETURNING VALUE(r)          TYPE REF TO data.

    CLASS-METHODS detranspose_structure
      IMPORTING i_field_value TYPE tt_field_value
      RETURNING VALUE(r)      TYPE REF TO data.

    CLASS-METHODS transpose_structure
      IMPORTING i_data   TYPE data
      RETURNING VALUE(r) TYPE tt_field_value.

    METHODS get_catalog
      RETURNING VALUE(r) TYPE abap_component_view_tab.

    METHODS add_index_column.

  PRIVATE SECTION.
    DATA _tab TYPE REF TO data.
    DATA _cat TYPE abap_component_view_tab.

    METHODS _update_tab.
ENDCLASS.


CLASS zcl_tbox_dyntab IMPLEMENTATION.
  METHOD add_column.
    CHECK NOT line_exists( _cat[ name = column_name ] ).

    DATA(comp_type) = COND #(
      WHEN like IS SUPPLIED     THEN cl_abap_typedescr=>describe_by_data( like )
      WHEN type IS SUPPLIED     THEN cl_abap_typedescr=>describe_by_name( type )
      WHEN type_ref IS SUPPLIED THEN type_ref
      ELSE                           cl_abap_typedescr=>describe_by_data( VALUE string( ) ) ).

    INSERT VALUE #( name = column_name
                    type = CAST cl_abap_datadescr( comp_type ) ) INTO TABLE _cat.

    _update_tab( ).
  ENDMETHOD.

  METHOD add_index_column.
    TYPES: BEGIN OF ts_index,
             _index TYPE i,
           END OF ts_index.
    DATA t_index TYPE TABLE OF ts_index WITH KEY _index.

    DO count( ) TIMES.
      INSERT VALUE #( _index = sy-index ) INTO TABLE t_index.
    ENDDO.

    extend_with_table( t_index ).
  ENDMETHOD.

  METHOD append.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    CHECK _tab IS BOUND.

    ASSIGN _tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(line) = lcl_helper=>create_type_struct_like_line( <tab> ).

    ASSIGN line->* TO FIELD-SYMBOL(<line>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING row TO <line>.
    APPEND <line> TO <tab>.
  ENDMETHOD.

  METHOD append_lines.
    LOOP AT tab ASSIGNING FIELD-SYMBOL(<row>).

      append( <row> ).

    ENDLOOP.
  ENDMETHOD.

  METHOD append_ref.
    CHECK row IS BOUND.
    ASSIGN row->* TO FIELD-SYMBOL(<row>).

    append( <row> ).
  ENDMETHOD.

  METHOD change_column_type.
    CHECK line_exists( _cat[ name = column_name ] ).

    DATA(comp_type) = COND #(
      WHEN like IS SUPPLIED     THEN cl_abap_typedescr=>describe_by_data( like )
      WHEN type IS SUPPLIED     THEN cl_abap_typedescr=>describe_by_name( type )
      WHEN type_ref IS SUPPLIED THEN type_ref
      ELSE                           cl_abap_typedescr=>describe_by_data( VALUE string( ) ) ).

    MODIFY TABLE _cat FROM VALUE #( name = column_name
                                    type = CAST cl_abap_elemdescr( comp_type ) ).

    _update_tab( ).

    r = me.
  ENDMETHOD.

  METHOD clear.
    CHECK _tab IS BOUND.

    ASSIGN _tab->* TO FIELD-SYMBOL(<tab>).

    CLEAR <tab>.
  ENDMETHOD.

  METHOD constructor.
    _tab = tab.
  ENDMETHOD.

  METHOD count.
    CHECK _tab IS BOUND.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    ASSIGN _tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    r = lines( <tab> ).
  ENDMETHOD.

  METHOD create_by_transposition.
    DATA(components) = lcl_helper=>get_components( tab ).

    IF NOT (     line_exists( components[ name = column_field ] )
             AND line_exists( components[ name = column_value ] ) ).
      RETURN.
    ENDIF.

    DELETE components WHERE name = column_field OR name = column_value.

    IF components IS INITIAL.

      DATA(temp_tab) = create_from_table( tab ).
      temp_tab->add_index_column( ).

      DATA(temp_tab_ref) = temp_tab->get( ).
      ASSIGN temp_tab_ref->* TO FIELD-SYMBOL(<temp_tab>).

      r = create_by_transposition( tab          = <temp_tab>
                                   column_field = column_field
                                   column_value = column_value
                                   prefix       = prefix ).

      r->del_column( `_INDEX` ).

      RETURN.

    ENDIF.

    DATA(dyn_tab) = create_from_catalog( components ).

    FIELD-SYMBOLS <dyn_ref_tab> TYPE ANY TABLE.

    DATA(dyn_ref) = dyn_tab->get( ).
    ASSIGN dyn_ref->* TO <dyn_ref_tab>.

    LOOP AT tab ASSIGNING FIELD-SYMBOL(<row>).

      DATA(row_key) = transpose_structure( <row> ).
      DELETE row_key WHERE field = column_field OR field = column_value.

      ASSIGN COMPONENT column_field OF STRUCTURE <row> TO FIELD-SYMBOL(<col>).
      dyn_tab->add_column( column_name = to_upper( |{ prefix }{ <col> }| ) ).

      IF row_key IS INITIAL.
        DATA(row) = dyn_tab->read_index( 1 ).
        IF row IS NOT BOUND.
          dyn_tab->append( <row> ).
          row = dyn_tab->read_index( 1 ).
        ENDIF.
      ELSE.
        row = dyn_tab->read_with_keys( row_key ).
        IF row IS NOT BOUND.
          dyn_tab->append( <row> ).
          row = dyn_tab->read_with_keys( row_key ).
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT column_value OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN row->* TO FIELD-SYMBOL(<row_mod>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT |{ prefix }{ <col> }| OF STRUCTURE <row_mod> TO FIELD-SYMBOL(<col_val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      <col_val> = <val>.

    ENDLOOP.

    r = dyn_tab.
  ENDMETHOD.

  METHOD create_empty.
    r = NEW #( ).
  ENDMETHOD.

  METHOD create_from_catalog.
    DATA(tab_ref) = lcl_helper=>create_type_table_from_catalog( catalog ).

    r = NEW #( tab_ref ).

    MOVE-CORRESPONDING catalog TO r->_cat.
  ENDMETHOD.

  METHOD create_from_table.
    DATA(catalog) = lcl_helper=>get_components( tab ).

    r = create_from_catalog( catalog ).

    r->append_lines( tab ).
  ENDMETHOD.

  METHOD del_column.
    CHECK line_exists( _cat[ name = column_name ] ).

    DELETE _cat WHERE name = column_name.

    _update_tab( ).
  ENDMETHOD.

  METHOD detranspose_structure.
    DATA(dyn_tab) = zcl_tbox_dyntab=>create_by_transposition( tab          = i_field_value
                                                              column_field = `FIELD`
                                                              column_value = `VALUE` ).

    r = dyn_tab->read_index( 1 ).
  ENDMETHOD.

  METHOD detranspose_table.
    LOOP AT i_tab_field_value INTO DATA(s_field_value).

      IF sy-tabix = 1.
        DATA(dyn_tab) = zcl_tbox_dyntab=>create_empty( ).
        LOOP AT s_field_value-row INTO DATA(column).
          dyn_tab->add_column( column-field ).
        ENDLOOP.
      ENDIF.

      DATA(dyn_row) = zcl_tbox_dyntab=>create_by_transposition( tab          = s_field_value-row
                                                                column_field = `FIELD`
                                                                column_value = `VALUE` ).

      DATA(row) = dyn_row->read_index( 1 ).

      dyn_tab->append_ref( row ).

    ENDLOOP.

    r = dyn_tab->get( ).
  ENDMETHOD.

  METHOD extend_catalog.
    DATA(new_components) = lcl_helper=>get_components( tab ).
    DATA(old_comp_rg)    = lcl_helper=>rangify( value = _cat tab_field = |NAME| emptiness = abap_true ).

    _cat = VALUE #( BASE _cat FOR new_comp IN new_components WHERE ( name NOT IN old_comp_rg ) ( new_comp ) ).

    _update_tab( ).
  ENDMETHOD.

  METHOD extend_with_table.
    extend_catalog( tab ).

    IF count( ) = 0.
      append_lines( tab ).
      RETURN.
    ENDIF.

    LOOP AT tab ASSIGNING FIELD-SYMBOL(<row>).

      DATA(new_row) = REF #( <row> ).
      DATA(old_row) = read_index( sy-tabix ).

      IF sy-tabix <= count( ).
        lcl_helper=>move_ref( from = new_row
                              to   = old_row ).
      ELSE.
        append_ref( new_row ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get.
    r = _tab.

    IF table IS NOT SUPPLIED.
      RETURN.
    ENDIF.

    lcl_helper=>dereference( EXPORTING data_ref = _tab IMPORTING data = table ).
  ENDMETHOD.

  METHOD get_catalog.
    r = _cat.
  ENDMETHOD.

  METHOD get_subtable.
    CHECK _tab IS BOUND.

    DATA(columns_rg) = lcl_helper=>rangify( columns ).

    DATA(dyn_sub) = create_from_catalog( VALUE #( FOR _column IN _cat WHERE ( name IN columns_rg ) ( _column ) ) ).

    ASSIGN _tab->* TO FIELD-SYMBOL(<tab>).

    dyn_sub->append_lines( <tab> ).

    r = dyn_sub->get( ).
  ENDMETHOD.

  METHOD is_initial.
    r = xsdbool( count( ) = 0 ).
  ENDMETHOD.

  METHOD pick_column.
    r = get_subtable( VALUE #( ( column ) ) ).
  ENDMETHOD.

  METHOD read_index.
    CHECK _tab IS BOUND.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN _tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE <tab> ASSIGNING FIELD-SYMBOL(<row>) INDEX index.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    r = REF #( <row> ).
  ENDMETHOD.

  METHOD read_with_condition.
    CHECK _tab IS BOUND.

    FIELD-SYMBOLS <tab>     TYPE ANY TABLE.
    FIELD-SYMBOLS <res_tab> TYPE ANY TABLE.

    ASSIGN _tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA r LIKE <tab>.
    IF r IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN r->* TO <res_tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>) WHERE (condition).

      INSERT <row> INTO TABLE <res_tab>.

    ENDLOOP.
  ENDMETHOD.

  METHOD read_with_keys.
    CHECK _tab IS BOUND.
    CHECK with_keys IS NOT INITIAL.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    ASSIGN _tab->* TO <tab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(str) = lcl_helper=>create_data_struct_from_values( with_keys ).
    ASSIGN str->* TO FIELD-SYMBOL(<str>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).

      IF lcl_helper=>structures_match( fields   = VALUE #( FOR _key IN with_keys ( _key-field ) )
                                       struct_1 = <row>
                                       struct_2 = <str> ) = abap_true.

        r = REF #( <row> ).

        RETURN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD transpose_structure.
    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( i_data ).
    IF data_desc->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.

    DATA(struct_desc) = CAST cl_abap_structdescr( data_desc ).

    DATA(components) = struct_desc->get_included_view( ).

    LOOP AT components INTO DATA(comp).

      IF comp-type->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT comp-name OF STRUCTURE i_data TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      INSERT VALUE #( field = comp-name
                      value = <val> ) INTO TABLE r.

    ENDLOOP.
  ENDMETHOD.

  METHOD transpose_table.
    r = VALUE #( FOR <row> IN i_data INDEX INTO i
                 ( index = i
                   row   = transpose_structure( <row> ) ) ).
  ENDMETHOD.

  METHOD _update_tab.
    DATA(new_tab) = lcl_helper=>create_type_table_from_catalog( _cat ).

    IF NOT is_initial( ).
      lcl_helper=>move_ref( from = _tab
                            to   = new_tab ).
    ENDIF.

    _tab = new_tab.
  ENDMETHOD.
ENDCLASS.
