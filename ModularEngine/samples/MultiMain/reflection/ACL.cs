public class ACL
{
	public static string getParent(string o)
	{
		int i = o.LastIndexOf('/');
		if (i < 0) return null;
		return o.Substring(0, i);
	}
	public static bool notOwned(string o)
	{
		return owns(o) == null;
	}
	public static string[] owns(string o)
	{
		switch (o) {
			case "i1": return new string[] { "g1" };
			case "i1/i2/i3": return new string[] { "g3" };
			default: return null;
		}
	}
}