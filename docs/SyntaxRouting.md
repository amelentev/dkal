# Routing Table Syntax Guide 
Routing tables are used by the single principal front ends to establish the network binding with the rest of the principals. 

A routing table is an XML file with a ".rt" file extension and conforms to the following XML schema:
{{
<?xml version="1.0" encoding="utf-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="RoutingTable">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="Principal" minOccurs="0" maxOccurs="unbounded">
          <xs:complexType>
            <xs:attribute name="name" type="xs:string" />
            <xs:attribute name="address" type="xs:string" />
          </xs:complexType>
        </xs:element>
      </xs:sequence>
      <xs:attribute name="me" type="xs:string" />
      <xs:attribute name="address" type="xs:string" />
    </xs:complexType>
  </xs:element>
</xs:schema>
}}
For instance:
{{
<?xml version="1.0" encoding="utf-8" ?>
<RoutingTable me="org1" address="http://localhost:55330/Dkal/PrincipalService">
  <Principal name="site1" address="http://foo.bar.org:55330/Site1" />
  <Principal name="site2" address="http://klomp.pruliu.com:55330/Site2" />
</RoutingTable>
}} Here the routing table specifies that:
* The current principal is named {{org1}}
* The current principal listens for incoming communication at {{http://localhost:55330/Dkal/PrincipalService}}
* There is another principal called {{site1}} that listens for incoming communications at {{http://foo.bar.org:55330/Site1}}
* There is another principal called {{site2}} that listens for incoming communications at {{http://klomp.pruliu.com:55330/Site2}}
