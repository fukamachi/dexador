"""Add Via and forward decrypted HTTPS requests to the HTTP test origin."""

from mitmproxy import http


def request(flow: http.HTTPFlow) -> None:
    via = flow.request.headers.get("Via")
    flow.request.headers["Via"] = f"{via}, 1.1 mitmproxy" if via else "1.1 mitmproxy"
    if flow.request.scheme == "https":
        flow.request.scheme = "http"
