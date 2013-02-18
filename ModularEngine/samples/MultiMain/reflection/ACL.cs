using System.Linq;
using System.Collections.Generic;
public class ACL
{
	private static Dictionary<string, string> obj2group = new Dictionary<string,string>()
	{
		/*
		          i1,g1
		        /       \
		  i21,g2          i22
		 /     \         /    \
		i31     i32    i33,g3  i34
		 */
		{"i1", "g1"}, 
		{"i1/i21", "g2"},
		{"i1/i21/i31", null},
		{"i1/i21/i32", null},

		{"i1/i22", null},
		{"i1/i22/i33", "g3"},
		{"i1/i22/i34", null},
	};
	
	public static string getParent(string o)
	{
		int i = o.LastIndexOf('/');
		return i<0 ? null : o.Substring(0, i);
	}
	public static bool notOwned(string o)
	{
		return owns(o) == null;
	}
	public static string owns(string o)
	{
		return obj2group.ContainsKey(o) ? obj2group[o] : null;
	}

	private static ILookup<string, string> children = obj2group.ToLookup(t => getParent(t.Key), t => t.Key);
	public static IEnumerable<string> getChildren(string o)
	{
		return children.Contains(o) ? children[o] : Enumerable.Empty<string>();
	}
	private static ILookup<string, string> group2obj = obj2group.ToLookup(t => t.Value, t => t.Key);
	public static IEnumerable<string> getObjectsIOwn(string g)
	{
		return group2obj.Contains(g) ? group2obj[g] : Enumerable.Empty<string>();
	}

	public static IEnumerable<string> getPermittedObjects(string g)
	{
		foreach (string o in getObjectsIOwn(g))
			foreach (string r in getPermittedObjectFrom(g, o))
				yield return r;
	}
	private static IEnumerable<string> getPermittedObjectFrom(string g, string o)
	{
		yield return o;
		foreach (string c in getChildren(o))
			if (notOwned(c))
				foreach (string r in getPermittedObjectFrom(g, c))
					yield return r;
	}
}
