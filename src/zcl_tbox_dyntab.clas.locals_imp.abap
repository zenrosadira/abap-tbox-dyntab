CLASS lcl_helper DEFINITION
  ABSTRACT
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_str_range TYPE RANGE OF string.

    CLASS-METHODS rangify
      IMPORTING !value    TYPE data
                tab_field TYPE clike     OPTIONAL
                emptiness TYPE abap_bool OPTIONAL
      RETURNING VALUE(r)  TYPE tt_str_range.

    CLASS-METHODS dereference
      IMPORTING data_ref TYPE REF TO data
      EXPORTING !data    TYPE data.

    CLASS-METHODS create_type_table_from_catalog
      IMPORTING catalog  TYPE abap_component_view_tab
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_type_table_of_structure
      IMPORTING !line    TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_type_table_of
      IMPORTING !line    TYPE REF TO data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_type_struct_from_cat
      IMPORTING catalog  TYPE abap_component_view_tab
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_type_struct_like_line
      IMPORTING !data    TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_data_table_of_strings
      IMPORTING !data    TYPE data
                shallow  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_data_struct_of_strings
      IMPORTING !data    TYPE data
                shallow  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_data_struct_from_values
      IMPORTING !values  TYPE zcl_tbox_dyntab=>tt_field_value
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_like_shallow
      IMPORTING !data    TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS create_like
      IMPORTING !data    TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS serialize
      IMPORTING !data    TYPE data
      RETURNING VALUE(r) TYPE string.

    CLASS-METHODS deserialize
      IMPORTING !xml  TYPE string
      EXPORTING !data TYPE data.

    CLASS-METHODS get_components
      IMPORTING !data    TYPE data
      RETURNING VALUE(r) TYPE abap_component_view_tab.

    CLASS-METHODS compose_structures
      IMPORTING struct_1 TYPE data
                struct_2 TYPE data
      RETURNING VALUE(r) TYPE REF TO data.

    CLASS-METHODS move_ref
      IMPORTING !from TYPE REF TO data
                !to   TYPE REF TO data.

    CLASS-METHODS structures_match
      IMPORTING struct_1 TYPE data
                struct_2 TYPE data
                !fields  TYPE string_table
      RETURNING VALUE(r) TYPE abap_bool.

  PRIVATE SECTION.
    DATA _object_kind         TYPE abap_typecategory.
    DATA _object_serialized   TYPE string.
    DATA _tabline_serialized  TYPE string.
    DATA _datatype_serialized TYPE string.

    METHODS _serialize
      IMPORTING !data TYPE data.

    METHODS _deserialize
      RETURNING VALUE(obj_ref) TYPE REF TO data.
ENDCLASS.


CLASS lcl_helper IMPLEMENTATION.
  METHOD create_data_struct_from_values.
    r = lcl_helper=>create_type_struct_from_cat(
            VALUE #( FOR val IN values
                     ( name = val-field
                       type = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( `STRING` ) ) ) ) ).

    ASSIGN r->* TO FIELD-SYMBOL(<str>).

    LOOP AT values INTO DATA(key_val).
      ASSIGN COMPONENT key_val-field OF STRUCTURE <str> TO FIELD-SYMBOL(<val>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      <val> = key_val-value.
    ENDLOOP.
  ENDMETHOD.

  METHOD rangify.
    " ---------------------------------------------------------------------
    " Use this method to get a range with values provided in input
    " VALUE parameter can be:
    "   - a simple element
    "   - a table with a simple element as row type
    "   - a table with a structure as row type, whose component
    "     of interest is provided in TAB_FIELD parameter
    "
    " ---------------------------------------------------------------------

    IF value IS INITIAL.

      IF emptiness = abap_true.
        r = VALUE #( ( sign   = `I`
                       option = `EQ`
                       low    = space ) ).
      ENDIF.

      RETURN.

    ENDIF.

    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( value ).

    CASE data_desc->kind.

      WHEN cl_abap_typedescr=>kind_elem.

        r = VALUE #( ( sign   = `I`
                       option = COND #( WHEN value CA `*` THEN `CP` ELSE `EQ` )
                       low    = value ) ).

      WHEN cl_abap_typedescr=>kind_table.

        FIELD-SYMBOLS <tab> TYPE ANY TABLE.
        ASSIGN value TO <tab>.

        DATA(tab_desc)  = CAST cl_abap_tabledescr( data_desc ).
        DATA(line_kind) = tab_desc->get_table_line_type( )->kind.

        CASE line_kind.

          WHEN cl_abap_typedescr=>kind_elem.
            r = VALUE #( FOR <val> IN <tab> ( sign = `I` option = `EQ` low = <val> ) ).

          WHEN cl_abap_typedescr=>kind_struct.
            LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<row>).
              ASSIGN COMPONENT tab_field OF STRUCTURE <row> TO FIELD-SYMBOL(<row_val>).
              IF sy-subrc = 0.
                r = VALUE #( BASE r ( sign = `I` option = `EQ` low = <row_val> ) ).
              ENDIF.
            ENDLOOP.

        ENDCASE.

    ENDCASE.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>COMPOSE_STRUCTURES
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRUCT_1                       TYPE        DATA
* | [--->] STRUCT_2                       TYPE        DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD compose_structures.
    DATA(comp_1) = get_components( struct_1 ).
    DATA(comp_2) = get_components( struct_2 ).

    DATA components LIKE comp_1.

    components = comp_1.

    APPEND LINES OF comp_2 TO components.

    SORT components BY name.
    DELETE ADJACENT DUPLICATES FROM components COMPARING name.

    r = create_type_struct_from_cat( components ).

    ASSIGN r->* TO FIELD-SYMBOL(<r>).

    MOVE-CORRESPONDING struct_1 TO <r>.
    MOVE-CORRESPONDING struct_2 TO <r>.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_DATA_STRUCT_OF_STRINGS
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [--->] SHALLOW                        TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_data_struct_of_strings.
    CHECK cl_abap_typedescr=>describe_by_data( data )->kind = cl_abap_typedescr=>kind_struct.

    DATA(components) = lcl_helper=>get_components( data ).
    IF components IS INITIAL.
      RETURN.
    ENDIF.

    DATA comps_str LIKE components.
    DATA comp_str  LIKE LINE OF comps_str.
    LOOP AT components INTO DATA(comp).

      CLEAR comp_str.
      comp_str-name = comp-name.

      CASE comp-type->kind.
        WHEN cl_abap_typedescr=>kind_elem.
          comp_str-type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( |STRING| ) ).
        WHEN cl_abap_typedescr=>kind_struct.
          ASSIGN COMPONENT comp-name OF STRUCTURE data TO FIELD-SYMBOL(<sub_comp>).
          DATA(sub_comp) = lcl_helper=>create_data_struct_of_strings( <sub_comp> ).
          comp_str-type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data_ref( sub_comp ) ).
        WHEN cl_abap_typedescr=>kind_table.
          ASSIGN COMPONENT comp-name OF STRUCTURE data TO FIELD-SYMBOL(<sub_tab>).
          DATA(sub_tab) = lcl_helper=>create_data_table_of_strings( <sub_tab> ).
          comp_str-type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data_ref( sub_tab ) ).
      ENDCASE.

      APPEND comp_str TO comps_str.

    ENDLOOP.

    r = lcl_helper=>create_type_struct_from_cat( CORRESPONDING #( comps_str ) ).

    IF shallow = abap_true.
      RETURN.
    ENDIF.
    IF r IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN r->* TO FIELD-SYMBOL(<data_str>).

    MOVE-CORRESPONDING data TO <data_str>.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_DATA_TABLE_OF_STRINGS
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [--->] SHALLOW                        TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_data_table_of_strings.
    CHECK cl_abap_typedescr=>describe_by_data( data )->kind = cl_abap_typedescr=>kind_table.

    DATA(line) = lcl_helper=>create_type_struct_like_line( data ).

    DATA(line_string) = lcl_helper=>create_data_struct_of_strings( line ).

    r = lcl_helper=>create_type_table_of( line_string ).

    IF shallow = abap_true.
      RETURN.
    ENDIF.
    IF r IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN r->* TO FIELD-SYMBOL(<tab>).

    MOVE-CORRESPONDING data TO <tab>.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_LIKE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_like.
    r = create_like_shallow( data ).
    ASSIGN r->* TO FIELD-SYMBOL(<r>).

    <r> = data.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_LIKE_SHALLOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_like_shallow.
    CREATE DATA r LIKE data.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_TYPE_STRUCT_FROM_CAT
* +-------------------------------------------------------------------------------------------------+
* | [--->] CATALOG                        TYPE        ABAP_COMPONENT_VIEW_TAB
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_type_struct_from_cat.
    DATA(line_type) = cl_abap_structdescr=>get( CORRESPONDING #( catalog ) ).

    CREATE DATA r TYPE HANDLE line_type.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_TYPE_STRUCT_LIKE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_type_struct_like_line.
    CHECK cl_abap_typedescr=>describe_by_data( data )->kind = cl_abap_typedescr=>kind_table.

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.
    ASSIGN data TO <tab>.

    CREATE DATA r LIKE LINE OF <tab>.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_TYPE_TABLE_FROM_CATALOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] CATALOG                        TYPE        ABAP_COMPONENT_VIEW_TAB
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_type_table_from_catalog.
    CHECK catalog IS NOT INITIAL.

    DATA(line_type) = cl_abap_structdescr=>get( CORRESPONDING #( catalog ) ).

    DATA(tab_type) = cl_abap_tabledescr=>get( p_line_type = line_type
                                              p_key_kind  = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA r TYPE HANDLE tab_type.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_TYPE_TABLE_OF
* +-------------------------------------------------------------------------------------------------+
* | [--->] LINE                           TYPE REF TO DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_type_table_of.
    CHECK line IS BOUND.

    DATA(tab_line_desc) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data_ref( line ) ).

    DATA(tab_type) = cl_abap_tabledescr=>get( p_line_type = tab_line_desc
                                              p_key_kind  = cl_abap_tabledescr=>keydefkind_empty ).

    CREATE DATA r TYPE HANDLE tab_type.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>CREATE_TYPE_TABLE_OF_STRUCTURE
* +-------------------------------------------------------------------------------------------------+
* | [--->] LINE                           TYPE        DATA
* | [<-()] R                              TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_type_table_of_structure.
    CHECK cl_abap_typedescr=>describe_by_data( line )->kind = cl_abap_typedescr=>kind_struct.

    r = create_type_table_of( REF #( line ) ).
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>DEREFERENCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA_REF                       TYPE REF TO DATA
* | [<---] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD dereference.
    CHECK data_ref IS BOUND.
    ASSIGN data_ref->* TO FIELD-SYMBOL(<data>).

    MOVE-CORRESPONDING <data> TO data.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] XML                            TYPE        STRING
* | [<---] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD deserialize.
    CALL TRANSFORMATION id
         SOURCE XML xml
         RESULT result = data.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>GET_COMPONENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [<-()] R                              TYPE        ABAP_COMPONENT_VIEW_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_components.
    DATA(data_desc) = cl_abap_typedescr=>describe_by_data( data ).

    CASE data_desc->kind.

      WHEN cl_abap_typedescr=>kind_table.

        DATA(tab_desc) = CAST cl_abap_tabledescr( data_desc ).
        DATA(tab_line) = tab_desc->get_table_line_type( ).

        CASE tab_line->kind.

          WHEN cl_abap_typedescr=>kind_struct.
            DATA(line_desc) = CAST cl_abap_structdescr( tab_line ).
            r = line_desc->get_included_view( ).

          WHEN OTHERS.
            r = VALUE #( ( name = |TABLE_LINE|
                           type = tab_line ) ).

        ENDCASE.

      WHEN cl_abap_typedescr=>kind_struct.

        DATA(struct_desc) = CAST cl_abap_structdescr( data_desc ).
        r = struct_desc->get_included_view( ).

    ENDCASE.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>MOVE_REF
* +-------------------------------------------------------------------------------------------------+
* | [--->] FROM                           TYPE REF TO DATA
* | [<---] TO                             TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD move_ref.
    IF from IS NOT BOUND OR to IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN from->* TO FIELD-SYMBOL(<from>).
    ASSIGN to->*   TO FIELD-SYMBOL(<to>).

    MOVE-CORRESPONDING <from> TO <to>.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* | [<-()] R                              TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD serialize.
    CALL TRANSFORMATION id
         SOURCE result = data
         RESULT XML r
         OPTIONS
           data_refs       = 'heap-or-create'
           technical_types = 'ignore'.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method lcl_helper=>STRUCTURES_MATCH
* +-------------------------------------------------------------------------------------------------+
* | [--->] STRUCT_1                       TYPE        DATA
* | [--->] STRUCT_2                       TYPE        DATA
* | [--->] FIELDS                         TYPE        STRING_TABLE
* | [<-()] R                              TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD structures_match.
    LOOP AT fields INTO DATA(field).

      ASSIGN COMPONENT field OF STRUCTURE struct_1 TO FIELD-SYMBOL(<val_1>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      ASSIGN COMPONENT field OF STRUCTURE struct_2 TO FIELD-SYMBOL(<val_2>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF <val_1> <> <val_2>.
        RETURN.
      ENDIF.

    ENDLOOP.

    r = abap_true.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_helper->_DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] OBJ_REF                        TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _deserialize.
    CASE _object_kind.

      WHEN cl_abap_typedescr=>kind_table.

        DATA tab_line TYPE REF TO data.

        lcl_helper=>deserialize( EXPORTING xml  = _tabline_serialized
                                 IMPORTING data = tab_line ).

        obj_ref = lcl_helper=>create_type_table_of( tab_line ).

        ASSIGN obj_ref->* TO FIELD-SYMBOL(<tab>).

        lcl_helper=>deserialize( EXPORTING xml  = _object_serialized
                                 IMPORTING data = <tab> ).

      WHEN OTHERS.

        lcl_helper=>deserialize( EXPORTING xml  = _datatype_serialized
                                 IMPORTING data = obj_ref ).

        ASSIGN obj_ref->* TO FIELD-SYMBOL(<data>).

        lcl_helper=>deserialize( EXPORTING xml  = _object_serialized
                                 IMPORTING data = <data> ).

    ENDCASE.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method lcl_helper->_SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] DATA                           TYPE        DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD _serialize.
    _object_serialized = lcl_helper=>serialize( data ).

    CASE _object_kind.

      WHEN cl_abap_typedescr=>kind_table.

        DATA(tab_line) = lcl_helper=>create_type_struct_like_line( data ).

        _tabline_serialized = lcl_helper=>serialize( tab_line ).

      WHEN OTHERS.

        DATA(data_type) = lcl_helper=>create_like_shallow( data ).

        _datatype_serialized = lcl_helper=>serialize( data_type ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
